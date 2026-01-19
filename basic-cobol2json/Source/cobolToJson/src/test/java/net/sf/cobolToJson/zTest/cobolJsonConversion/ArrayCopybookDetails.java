package net.sf.cobolToJson.zTest.cobolJsonConversion;

import net.sf.JRecord.Details.AbstractLine;

public class ArrayCopybookDetails {

	public static class SimpleArray implements IArrayCopybook {

		@Override
		public String getCopybookString() {
			return ""
			+ "       01  Test-Copybook.\n"
			+ "           03 field-1           PIC X(10).\n"
			+ "           03 Array-1  OCCURS 5 PIC 9(5).\n"
			+ "           03 field-2           PIC x(10).\n"
			+ "";
		}

		@Override
		public void updateLine(AbstractLine line, int lineNumber) {
			
			line.getFieldValue("field-1").set("Field a " + lineNumber);
			for (int i = 0; i < 5; i++) {
				line.getFieldValue("Array-1 (" + i + ")").set(lineNumber*100 + i);
			}
			line.getFieldValue("field-2").set("Field b " + lineNumber);
		}

		@Override
		public String getExpectedSingleRecordJson() {
			return ""
			+ "{\n"
			+ "  \"Test_Copybook\" : {\n"
			+ "    \"field_1\" : \"Field a 1\",\n"
			+ "    \"Array_1\" : [ 100, 101, 102, 103, 104 ],\n"
			+ "    \"field_2\" : \"Field b 1\"\n"
			+ "  }\n"
			+ "}";
		}

		@Override
		public String getJsonArray() {
			return ""
			+ "[ {\n"
			+ "    \"field_1\" : \"Field a 1\",\n"
			+ "    \"Array_1\" : [ 100, 101, 102, 103, 104 ],\n"
			+ "    \"field_2\" : \"Field b 1\"\n"
			+ "  }, {\n"
			+ "    \"field_1\" : \"Field a 2\",\n"
			+ "    \"Array_1\" : [ 200, 201, 202, 203, 204 ],\n"
			+ "    \"field_2\" : \"Field b 2\"\n"
			+ "  }, {\n"
			+ "    \"field_1\" : \"Field a 3\",\n"
			+ "    \"Array_1\" : [ 300, 301, 302, 303, 304 ],\n"
			+ "    \"field_2\" : \"Field b 3\"\n"
			+ "  } ]\n";
		}

		@Override
		public String getExpectedJsonArray() {
			return ""
			+ "{\n"
			+ "  \"Test_Copybook\" : " + getJsonArray()
			+ "}"
			;
		}
		
	}
	

	public static class TwoDimensionalArray implements IArrayCopybook {

		@Override
		public String getCopybookString() {
			return ""
			+ "       01  Test-Copybook.\n"
			+ "           03 field-1           PIC X(10).\n"
			+ "           03 Array-1  OCCURS 2.\n"
			+ "              05 Array-1-1    OCCURS 3  PIC 9(5).\n"
			+ "           03 field-2           PIC x(10).\n"
			+ "";
		}

		@Override
		public void updateLine(AbstractLine line, int lineNumber) {
			line.getFieldValue("field-1").set("Field a " + lineNumber);
			for (int i1 = 0; i1 < 2; i1++) {
				for (int i2 = 0; i2 < 3; i2++) {
					line.getFieldValue("Array-1-1 (" + i1 + ", " + i2 + ")").set(lineNumber*100 + i1 * 10 + i2);
				}
			}
			line.getFieldValue("field-2").set("Field b " + lineNumber);
		}

		@Override
		public String getExpectedSingleRecordJson() {
			return ""
			+ "{\n"
			+ "  \"Test_Copybook\" : {\n"
			+ "    \"field_1\" : \"Field a 1\",\n"
			+ "    \"Array_1\" : [ {\n"
			+ "      \"Array_1_1\" : [ 100, 101, 102 ]\n"
			+ "    }, {\n"
			+ "      \"Array_1_1\" : [ 110, 111, 112 ]\n"
			+ "    } ],\n"
			+ "    \"field_2\" : \"Field b 1\"\n"
			+ "  }\n"
			+ "}";
		}

		@Override
		public String getJsonArray() {
			return ""
			+ "[ {\n"
			+ "    \"field_1\" : \"Field a 1\",\n"
			+ "    \"Array_1\" : [ {\n"
			+ "      \"Array_1_1\" : [ 100, 101, 102 ]\n"
			+ "    }, {\n"
			+ "      \"Array_1_1\" : [ 110, 111, 112 ]\n"
			+ "    } ],\n"
			+ "    \"field_2\" : \"Field b 1\"\n"
			+ "  }, {\n"
			+ "    \"field_1\" : \"Field a 2\",\n"
			+ "    \"Array_1\" : [ {\n"
			+ "      \"Array_1_1\" : [ 200, 201, 202 ]\n"
			+ "    }, {\n"
			+ "      \"Array_1_1\" : [ 210, 211, 212 ]\n"
			+ "    } ],\n"
			+ "    \"field_2\" : \"Field b 2\"\n"
			+ "  }, {\n"
			+ "    \"field_1\" : \"Field a 3\",\n"
			+ "    \"Array_1\" : [ {\n"
			+ "      \"Array_1_1\" : [ 300, 301, 302 ]\n"
			+ "    }, {\n"
			+ "      \"Array_1_1\" : [ 310, 311, 312 ]\n"
			+ "    } ],\n"
			+ "    \"field_2\" : \"Field b 3\"\n"
			+ "  } ]"
			;
		}

		@Override
		public String getExpectedJsonArray() {
			return ""
			+ "{\n"
			+ "  \"Test_Copybook\" : " + getJsonArray() + "\n"
			+ "}"
			;

		}
		
	}


	public static class ComplexArray implements IArrayCopybook {

		@Override
		public String getCopybookString() {
			return ""
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
		}

		@Override
		public void updateLine(AbstractLine line, int lineNumber) {
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
		}

		@Override
		public String getExpectedSingleRecordJson() {
			return ""
			+ "{\n"
			+ "  \"Test_Copybook\" : {\n"
			+ "    \"field_1\" : \"Field a 1\",\n"
			+ "    \"Array_1\" : [ {\n"
			+ "      \"field_1_2\" : \"b 10\",\n"
			+ "      \"Array_1_2\" : [ 100, 101, 102 ],\n"
			+ "      \"Array_1_3\" : [ {\n"
			+ "        \"field_1_3_1\" : \"c 100\",\n"
			+ "        \"field_1_3_2\" : \"d- 100\"\n"
			+ "      }, {\n"
			+ "        \"field_1_3_1\" : \"c 101\",\n"
			+ "        \"field_1_3_2\" : \"d- 101\"\n"
			+ "      } ]\n"
			+ "    }, {\n"
			+ "      \"field_1_2\" : \"b 11\",\n"
			+ "      \"Array_1_2\" : [ 110, 111, 112 ],\n"
			+ "      \"Array_1_3\" : [ {\n"
			+ "        \"field_1_3_1\" : \"c 110\",\n"
			+ "        \"field_1_3_2\" : \"d- 110\"\n"
			+ "      }, {\n"
			+ "        \"field_1_3_1\" : \"c 111\",\n"
			+ "        \"field_1_3_2\" : \"d- 111\"\n"
			+ "      } ]\n"
			+ "    } ],\n"
			+ "    \"field_2\" : \"Field e 1\"\n"
			+ "  }\n"
			+ "}";
		}

		@Override
		public String getJsonArray() {
			return null;
		}

		@Override
		public String getExpectedJsonArray() {
			return null;
		}
		
	}

}
