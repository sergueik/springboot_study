package net.sf.cobolToJson.zTest.cobolJsonConversion;

import static org.junit.Assert.assertEquals;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import net.sf.JRecord.Details.AbstractLine;

public class RunJsonToCobolConversionTests {
	private final ICobolJsonConversionDetails cobolJsonconversion;

	final ArrayList<AbstractLine> lines;
	String jsonString;

	public RunJsonToCobolConversionTests(ICobolJsonConversionDetails cobolJsonconversion) throws IOException {
		super();
		this.cobolJsonconversion = cobolJsonconversion;
		lines = createLines(4);
	}
	
	public void testSingleObject() throws IOException {
		AbstractLine line = cobolJsonconversion.newLine(1);
		
		String jsonString = cobolJsonconversion.singleRecordToJsonObject(line);
		
		String expectedJson = cobolJsonconversion.getCobolCopybookDetails().getExpectedSingleRecordJson();
		if (expectedJson == null) {
			System.out.println(jsonString);
		} else {
			assertEquals(expectedJson, jsonString);
		}
		
		AbstractLine convertedLine = cobolJsonconversion.jsonObjectToSingleRecord(jsonString);
		
		assertEquals(line.getFullLine(), convertedLine.getFullLine());
	}



	public void testMultipleRecords() throws IOException {
		checkGeneratedJSon();
		
		IArrayCopybook cobolDetails = cobolJsonconversion.getCobolCopybookDetails();
		String jsonArrayString = cobolDetails.getJsonArray();
		if (jsonArrayString != null) {
			checkJsonString("{ \"Lines\":" + jsonArrayString + "}");
			checkJsonString(jsonArrayString);
		}
		
		String expectedJsonArray = cobolDetails.getExpectedJsonArray();
		if (expectedJsonArray != null) {
			assertEquals(expectedJsonArray, jsonString);
		}

	}
	
	void checkGeneratedJSon() throws IOException {
		ByteArrayInputStream is = createInputStream(lines);
		
		jsonString = cobolJsonconversion.multipleRecordsToJsonArray(is);

		checkJsonString(jsonString);
	}


	
	private void checkJsonString(String jsonString) throws IOException {
		checkGeneratedLines(
				lines,
				cobolJsonconversion.jsonArrayToMultipleRecords(jsonString));
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


	private ArrayList<AbstractLine> createLines(int size) throws IOException {
		ArrayList<AbstractLine> lines = new ArrayList<>();

		for (int i = 1; i < size; i++) {
			lines.add( cobolJsonconversion.newLine(i));
		}
		return lines;
	}

}
