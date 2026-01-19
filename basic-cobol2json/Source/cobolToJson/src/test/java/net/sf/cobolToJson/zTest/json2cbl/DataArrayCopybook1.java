package net.sf.cobolToJson.zTest.json2cbl;

import java.io.IOException;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.cb2xml.copybookReader.CobolCopybookSource;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class DataArrayCopybook1 implements ITestData {
	public static ITestData new2DiminesionArrayData() {
		return new Array2Dimension();
	}
	public static ITestData newComplexArrayData() {
		return new ArrayComplicated();
	}
	
	private static String copybook1 = ""
			+ "       01  Test-Copybook.\n"
			+ "           03 field-1           PIC X(10).\n"
			+ "           03 Array-1  OCCURS 5 PIC 9(5).\n"
			+ "           03 field-2           PIC x(10).\n"
			+ "";

	private static ICobol2Json getCopybook1(String copybookStr) {
		return Cobol2Json.newCobol2Json(new CobolCopybookSource("Test-Copybook", copybookStr))
		  .setFileOrganization(IFileStructureConstants.IO_STANDARD_TEXT_FILE)
		  .setSplitCopybook(CopybookLoader.SPLIT_NONE)
		  .setFont("")
		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
		  ;
	}
	
	private final ICobol2Json cobol2Json = getCopybook1(copybook1);

	@Override
	public ICobol2Json getCobol2Json() {
		return cobol2Json;
	}
	
	@Override
	public AbstractLine cobolDataLine(int lineNumber) throws IOException {
		AbstractLine line = cobol2Json.asIOBuilder().newLine();
		
		line.getFieldValue("field-1").set("Field a " + lineNumber);
		for (int i = 0; i < 5; i++) {
			line.getFieldValue("Array-1 (" + i + ")").set(lineNumber*100 + i);
		}
		line.getFieldValue("field-2").set("Field b " + lineNumber);
		
		return line;
	}
	
	private static class Array2Dimension implements ITestData {
		private static String copybook2 = ""
				+ "       01  Test-Copybook.\n"
				+ "           03 field-1           PIC X(10).\n"
				+ "           03 Array-1  OCCURS 2.\n"
				+ "              05 Array-1-1    OCCURS 3  PIC 9(5).\n"
				+ "           03 field-2           PIC x(10).\n"
				+ "";
		private final ICobol2Json cobol2Json = getCopybook1(copybook2);
		
		@Override
		public ICobol2Json getCobol2Json() {
			return cobol2Json;
		}
		
		@Override
		public AbstractLine cobolDataLine(int lineNumber) throws IOException {
			AbstractLine line = cobol2Json.asIOBuilder().newLine();
			
			line.getFieldValue("field-1").set("Field a " + lineNumber);
			for (int i1 = 0; i1 < 2; i1++) {
				for (int i2 = 0; i2 < 3; i2++) {
					line.getFieldValue("Array-1-1 (" + i1 + ", " + i2 + ")").set(lineNumber*100 + i1 * 10 + i2);
				}
			}
			line.getFieldValue("field-2").set("Field b " + lineNumber);
			
			return line;
		}


	}
	
	private static class ArrayComplicated implements ITestData {
		private static String copybook = ""
				+ "       01  Test-Copybook.\n"
				+ "           03 field-1             PIC X(10).\n"
				+ "           03 Array-1  OCCURS 2.\n"
				+ "              05 field-1-2        pic x(4)."
				+ "              05 Array-1-2  OCCURS 3  PIC 9(5).\n"
				+ "              05 Array-1-3  OCCURS 2.\n"
				+ "                 07 field-1-3-1  pic x(5)."
				+ "                 07 field-1-3-2  pic x(6)."
				+ "           03 field-2            PIC x(10).\n"
				+ "";
		private final ICobol2Json cobol2Json = getCopybook1(copybook);
		
		@Override
		public ICobol2Json getCobol2Json() {
			return cobol2Json;
		}
		
		@Override
		public AbstractLine cobolDataLine(int lineNumber) throws IOException {
			AbstractLine line = cobol2Json.asIOBuilder().newLine();
			
			line.getFieldValue("field-1").set("Field a " + lineNumber);
			for (int i1 = 0; i1 < 2; i1++) {
				line.getFieldValue("field-1-2 (" + i1 + ")").set("b " + lineNumber + "" + i1);
				for (int i2 = 0; i2 < 3; i2++) {
					line.getFieldValue("Array-1-2 (" + i1 + ", " + i2 + ")").set(lineNumber*100 + i1 * 10 + i2);
				}
				for (int i2 = 0; i2 < 2; i2++) {
					line.getFieldValue("field-1-3-1 (" + i1 + ", " + i2 + ")").set("c " + (lineNumber*100 + i1 * 10 + i2));
					line.getFieldValue("field-1-3-2 (" + i1 + ", " + i2 + ")").set("d- " +(lineNumber*100 + i1 * 10 + i2));
				}
			}
			line.getFieldValue("field-2").set("Field e " + lineNumber);
			
			return line;
		}


	}
}
