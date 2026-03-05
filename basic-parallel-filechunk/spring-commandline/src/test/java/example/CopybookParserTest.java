package example;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
// NOTE: JRecord APIs vary significantly between releases.
// The following import may need adjustment:
//import net.sf.JRecord.External.LayoutDetail;
import net.sf.JRecord.Details.LayoutDetail;

import net.sf.JRecord.Common.Constants;

import net.sf.JRecord.Numeric.ICopybookDialects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;

import java.util.stream.Stream;
import java.util.Arrays;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

/*
 * WARNING:
 * This test class uses @TestInstance(PER_CLASS) so that @MethodSource
 * providers can access Spring-injected configuration (ExampleProperties).
 *
 * If this lifecycle constraint causes issues when integrating into a larger test suite
 * or parallel execution environment, revert to commit <commit-id> where
 * copybook paths were hard-coded and MethodSource providers were static.
 */
 
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest
class CopybookParserTest {

	@Autowired
	private ExampleProperties exampleProperties;

	private ICobolIOBuilder iCobolIOBuilder = null;

	// NOTE: Maintains deterministic order — important for reproducible tests
	public final String[] badCopybooks() {

	    final String baseBadPath = exampleProperties.getBaseBadPath();

	    // NOTE:
	    // List.toArray() without arguments returns Object[].
	    // A blind cast to String[] is impossible:
	    //   class [Ljava.lang.Object; cannot be cast to class [Ljava.lang.String;
	    //
	    // Use the typed overload instead so Java creates a String[] directly.
	    // What the Argument Represents
	    // in the call 
	    // String[] array = list.toArray(new String[list.size()]);
	    // The argument new String[list.size()] is not merely a placeholder
	    // rather it defines the slot size and type of the target array
	    final String[] badCopybooks = exampleProperties.getBadCopybooks().toArray(new String[0]);

	    String[] filePaths = new String[badCopybooks.length];

	    for (int cnt = 0; cnt < badCopybooks.length; cnt++) {
	        filePaths[cnt] = baseBadPath + badCopybooks[cnt];
	    }

	    return filePaths;
	}
	
	public Stream<String> goodCopybooks() {
		final String baseGoodPath = exampleProperties.getBaseGoodPath();
		return exampleProperties.getGoodCopybooks().stream().map(fileName -> baseGoodPath + fileName);
	}

	@DisplayName("Fails on Simple crafted Copybook With the number not88 Error")
	@ParameterizedTest
	@MethodSource("badCopybooks")
	void testBadCopybooks(final String copybookPath) {
		iCobolIOBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookPath).setDialect(ICopybookDialects.FMT_MAINFRAME)
				.setFileOrganization(Constants.IO_FIXED_LENGTH).setFont("utf8");

		RecordException ex = assertThrows(RecordException.class, iCobolIOBuilder::getLayout);
		assertThat("Expected parser to throw exception", ex.getMessage(), containsString("number88"));
	}

	@DisplayName("Reads Copybook Without Error")
	@ParameterizedTest
	@MethodSource("goodCopybooks")
	void testGoodCopybooks(final String copybookPath) {
		iCobolIOBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookPath).setDialect(ICopybookDialects.FMT_MAINFRAME)
				.setFileOrganization(Constants.IO_FIXED_LENGTH).setFont("utf8");

		LayoutDetail layoutDetail = assertDoesNotThrow(iCobolIOBuilder::getLayout);
		assertThat("Expected parser to read layout detail", layoutDetail, notNullValue());
	}
}
