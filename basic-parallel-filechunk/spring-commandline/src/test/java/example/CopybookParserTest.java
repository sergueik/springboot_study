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
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.stream.Stream;
import java.util.Arrays;
import java.util.List;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

class CopybookParserTest {

	private static final String BASE_BAD_PATH = "src/test/resources/copybooks/bad/";
	private static final String BASE_GOOD_PATH = "src/test/resources/copybooks/good/";

	private ICobolIOBuilder iCobolIOBuilder = null;

	// NOTE: Maintains deterministic order — important for reproducible tests
	public final static String[] badCopybooks() {

		final String[] fileNames = new String[] { "bad-88-placement.cbl", "example-dc3aa31c.cbl", "garbage-token.cbl",
				"comp3-fields.cbl", "occurs-array.cbl", "simple-fixed.cbl", "with-88-levels.cbl", "missing-level.cbl" };
		String[] filePaths = new String[fileNames.length];
		for (int cnt = 0; cnt < fileNames.length; cnt++) {
			filePaths[cnt] = BASE_BAD_PATH + fileNames[cnt];
		}
		return filePaths;
	}

	public static Stream<String> goodCopybooks() {
		List<String> files = Arrays.asList("example.cbl");
		return files.stream().map(fileName -> BASE_GOOD_PATH + fileName);
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
