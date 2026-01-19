package net.sf.cobolToJson.zTest.cobolJsonConversion;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;

import net.sf.JRecord.Details.AbstractLine;

public interface ICobolJsonConversionDetails {

	AbstractLine newLine(int lineNumber) throws IOException;

	IArrayCopybook getCobolCopybookDetails();

	AbstractLine jsonObjectToSingleRecord(String jsonString) throws IOException;
	String singleRecordToJsonObject(AbstractLine line);
	
	String multipleRecordsToJsonArray(ByteArrayInputStream is) throws IOException;
	List<AbstractLine> jsonArrayToMultipleRecords(String jsonString) throws IOException;
	
}