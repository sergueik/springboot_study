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

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;

import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.CoreMatchers.containsString;

import static org.hamcrest.Matchers.is;

// import static org.junit.jupiter.api.Assertions.*;

import org.junit.jupiter.api.DisplayName;

class CopybookParserTest {

	private ICobolIOBuilder iCobolIOBuilder = null;
	private String copybookPath = null;

	@DisplayName("Fails on Simple crafted Copybook With the number not88 Error")
	@Test
	public void test1() {
		copybookPath = "src/test/resources/copybooks/bad/example-dc3aa31c.cbl";

		iCobolIOBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookPath).setDialect(ICopybookDialects.FMT_MAINFRAME)
				.setFileOrganization(Constants.IO_FIXED_LENGTH).setFont("utf8");

		RecordException ex = assertThrows(RecordException.class, iCobolIOBuilder::getLayout);

		assertThat("Expected parser to throw exception", ex.getMessage(), containsString("number88"));
	}

	@DisplayName("Reads Copybook Without Error")
	@Test
	void test2() throws IOException {
		copybookPath = "src/test/resources/copybooks/good/example.cbl";

		iCobolIOBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookPath).setDialect(ICopybookDialects.FMT_MAINFRAME)
				.setFileOrganization(Constants.IO_FIXED_LENGTH).setFont("utf8");
		LayoutDetail layoutDetail = assertDoesNotThrow(iCobolIOBuilder::getLayout);
		assertThat("Expected parser to reas layout detail", layoutDetail, notNullValue());
	}
}
