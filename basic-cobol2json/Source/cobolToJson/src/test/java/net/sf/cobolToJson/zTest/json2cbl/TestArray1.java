package net.sf.cobolToJson.zTest.json2cbl;

import static org.junit.Assert.*;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.cobolToJson.def.ICobol2Json;

public class TestArray1 {
	
	String singleRecord = ""
			+ "{\n"
			+ "  \"Test_Copybook\" : {\n"
			+ "    \"field_1\" : \"Field a 1\",\n"
			+ "    \"Array_1\" : [ 100, 101, 102, 103, 104 ],\n"
			+ "    \"field_2\" : \"Field b 1\"\n"
			+ "  }\n"
			+ "}";
	
	String multiRecord1 = ""
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
			+ "  } ]\n"
			;
	String multiRecord2 = ""
			+ "{\n"
			+ "  \"Test_Copybook\" : " + multiRecord1
			+ "}"
			;
	
	String singleRecord2Dim = ""
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
	
	String singleRecordComplicated = ""
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

	
	String multiRecord2Dim = ""
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

	String multiRecord2Dim2 = ""
			+ "{\n"
			+ "  \"Test_Copybook\" : " + multiRecord2Dim + "\n"
			+ "}"
			;

	@Test
	public void testObject1Dimension() throws IOException {
		testSingleObjectCreation(new DataArrayCopybook1(), singleRecord);
	}


	@Test
	public void testObject2Dimension() throws IOException {
		testSingleObjectCreation(DataArrayCopybook1.new2DiminesionArrayData(), singleRecord2Dim);
	}


	@Test
	public void testObjectComplicated() throws IOException {
		testSingleObjectCreation(DataArrayCopybook1.newComplexArrayData(), singleRecordComplicated);
	}

	private void testSingleObjectCreation(ITestData data, String expectedJson) throws IOException {
		ICobol2Json cobol2Json = data.getCobol2Json();
		
		AbstractLine line = data.cobolDataLine(1);
		
		String jsonString = cobol2Json.singleCobolRecord2jsonString(line.getData());
		
		if (expectedJson == null) {
			System.out.println(jsonString);
		} else {
			assertEquals(expectedJson, jsonString);
		}
		
		AbstractLine convertedLine = cobol2Json.jsonObjectToCobolLine(new StringReader(jsonString));
		
		assertEquals(line.getFullLine(), convertedLine.getFullLine());
	}

	
	@Test
	public void testArrayOfObjects() throws IOException {
		
		TestList test = new TestList(new DataArrayCopybook1());
		
		test.checkGeneratedJSon();
		test.checkJsonString("{ \"Lines\":" + multiRecord1 + "}");
		test.checkJsonString(multiRecord1);
		
		assertEquals(multiRecord2, test.jsonString);
	}
	
	@Test
	public void testArrayOfObjects2Dimension() throws IOException {
		
		TestList test = new TestList(DataArrayCopybook1.new2DiminesionArrayData());
		
		test.checkGeneratedJSon();
		//System.out.println(test.jsonString);
		test.checkJsonString("{ \"Lines\":" + multiRecord2Dim + "}");
		test.checkJsonString(multiRecord2Dim2);
		
		assertEquals(multiRecord2Dim2, test.jsonString);
	}
	
	@Test
	public void testArrayOfObjectsComplicated() throws IOException {
		
		TestList test = new TestList(DataArrayCopybook1.newComplexArrayData());
		
		test.checkGeneratedJSon();
		
		int st = test.jsonString.indexOf('[');
		String s = test.jsonString.substring(st);
		int en = s.lastIndexOf(']');
		s = s.substring(0, en+1);
//		System.out.println(st + "\t" + en + "\t" + s);
		
		test.checkJsonString(s);
		
	}

	private static class TestList {
		final ArrayList<AbstractLine> lines;
		
		final ICobol2Json cobol2Json;
		String jsonString;
		
		TestList(ITestData data) throws IOException  {
			lines = createLines(data, 4);
			
			cobol2Json = data.getCobol2Json();
		}
		
		void checkGeneratedJSon() throws IOException {
			ByteArrayInputStream is = createInputStream(lines);
			StringWriter w = new StringWriter();
			cobol2Json.cobol2json(is, w);
			
			jsonString = w.toString();

			checkJsonString(jsonString);
		}
		
		private void checkJsonString(String jsonString) throws IOException {
			checkGeneratedLines(lines, cobol2Json.jsonArrayToCobolLines(new StringReader(jsonString)));
		}


		private void checkGeneratedLines(ArrayList<AbstractLine> lines, List<AbstractLine> generatedLines) {
			int size = Math.min(lines.size(), generatedLines.size());

			for (int i = 0; i < size; i++) {
				assertEquals(lines.get(i).getFullLine(), generatedLines.get(i).getFullLine());
			}
			assertEquals(lines.size(), generatedLines.size());
		}



		private ByteArrayInputStream createInputStream(ArrayList<AbstractLine> lines) {
			StringBuilder b = new StringBuilder();
			for (AbstractLine l : lines) {
				b.append(l.getFullLine()).append('\n');
			}
			ByteArrayInputStream is = new ByteArrayInputStream(b.toString().getBytes());
			return is;
		}


		private ArrayList<AbstractLine> createLines(ITestData data, int size) throws IOException {
			ArrayList<AbstractLine> lines = new ArrayList<>();

			for (int i = 1; i < size; i++) {
				lines.add( data.cobolDataLine(i));
			}
			return lines;
		}
	}
}
