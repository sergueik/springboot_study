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

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest
class CopybookParserTest {
	@Value("${example.baseBadPath}")
	private String baseBadPath;

	@Value("${example.baseGoodPath}")
	private String baseGoodPath;

	@Value("#{'${example.badCopybooks}'.split(',')}")
	private String[] badCopybooks;

	@Value("#{'${example.goodCopybooks}'.split(',')}")
	private String[] goodCopybooks;

	private ICobolIOBuilder iCobolIOBuilder = null;

	// NOTE: Maintains deterministic order — important for reproducible tests
	public final String[] badCopybooks() {

		String[] filePaths = new String[badCopybooks.length];
		for (int cnt = 0; cnt < badCopybooks.length; cnt++) {
			filePaths[cnt] = baseBadPath + badCopybooks[cnt];
		}
		return filePaths;
	}

	public Stream<String> goodCopybooks() {
		return Arrays.stream(goodCopybooks).map(fileName -> baseGoodPath + fileName);
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
