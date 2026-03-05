package example;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
// import net.sf.JRecord.External.LayoutDetail;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Numeric.ICopybookDialects;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.util.stream.Stream;

import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.CoreMatchers.containsString;

import static org.hamcrest.Matchers.is;

// import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;

class CopybookParserTest {

	private ICobolIOBuilder iCobolIOBuilder = null;

	// @formatter:off
	public static String[] badCopybooks() {
		return new String[] { 
			"src/test/resources/copybooks/bad/bad-88-placement.cbl",
			"src/test/resources/copybooks/bad/example-dc3aa31c.cbl",
			"src/test/resources/copybooks/bad/garbage-token.cbl",
			"src/test/resources/copybooks/bad/comp3-fields.cbl",
			"src/test/resources/copybooks/bad/occurs-array.cbl",
			"src/test/resources/copybooks/bad/simple-fixed.cbl",
			"src/test/resources/copybooks/bad/with-88-levels.cbl",
			"src/test/resources/copybooks/bad/missing-level.cbl"
		};
	}
	// @formatter:on

	// @formatter:off
	public static Stream<String> goodCopybooksDataStream() {
		return Stream.of(
			"src/test/resources/copybooks/good/example.cbl"
		);
	}
	// @formatter:on

	@DisplayName("Fails on Simple crafted Copybook With the number not88 Error")
	@ParameterizedTest
	@MethodSource("badCopybooks")
	void test1(final String copybookPath) {

		iCobolIOBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookPath).setDialect(ICopybookDialects.FMT_MAINFRAME)
				.setFileOrganization(Constants.IO_FIXED_LENGTH).setFont("utf8");

		RecordException ex = assertThrows(RecordException.class, iCobolIOBuilder::getLayout);

		assertThat("Expected parser to throw exception", ex.getMessage(), containsString("number88"));
	}

	@DisplayName("Reads Copybook Without Error")
	@ParameterizedTest
	@MethodSource("goodCopybooksDataStream")
	void test2(final String copybookPath) {

		iCobolIOBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookPath).setDialect(ICopybookDialects.FMT_MAINFRAME)
				.setFileOrganization(Constants.IO_FIXED_LENGTH).setFont("utf8");
		LayoutDetail layoutDetail = assertDoesNotThrow(iCobolIOBuilder::getLayout);
		assertThat("Expected parser to reas layout detail", layoutDetail, notNullValue());
	}
}
