package net.sf.cobolToJson.zTest.cobolJsonConversion;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.List;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cobolToJson.CobolJsonConversion;
import net.sf.cobolToJson.def.ICobolJsonConversion;

public class CobolJsonConversionDetails implements ICobolJsonConversionDetails  {

	
//	private final ICobolRecordToJsonObjectIn cobolToJson;
//	private final IJsonObjectToCobolRecordIn jsonToCobol;
	private final ICobolJsonConversion cobolJsonConversion;
	public final IArrayCopybook cobolCopybookDetails;
	
	public CobolJsonConversionDetails(IArrayCopybook cobolCopybookDetails) throws IOException {
		this.cobolJsonConversion = CobolJsonConversion.newCobolJsonConversion(
				new ReadCobolCopybook()
					.setCopybookName("Test-Copybook")
					.addFreeFormatCobolText(cobolCopybookDetails.getCopybookString()))
		  .setFileOrganization(IFileStructureConstants.IO_STANDARD_TEXT_FILE)
		  .setSplitCopybook(CopybookLoader.SPLIT_NONE)
		  .setFont("")
		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
		  ;
		this.cobolCopybookDetails = cobolCopybookDetails;
	}
	

	@Override
	public IArrayCopybook getCobolCopybookDetails() {
		return cobolCopybookDetails;
	}

//
//	public ISchemaIOBuilder asIOBuilder() {
//		return cobolJsonConversion.asIOBuilder();
//	}



	@Override
	public AbstractLine newLine(int lineNumber) throws IOException {
		AbstractLine line = cobolJsonConversion.asIOBuilder().newLine();
		cobolCopybookDetails.updateLine(line, lineNumber);
		return line;
	}
	
	@Override
	public AbstractLine jsonObjectToSingleRecord(String jsonString) throws IOException {
		return cobolJsonConversion.jsonObjectToSingleRecord()
				.setJsonString(jsonString)
				.toCobolDataLine();
	}

	@Override
	public String singleRecordToJsonObject(AbstractLine line) {
		return cobolJsonConversion.singleRecordToJsonObject()
				.setCobolRecord(line.getData())
				.toJsonString();
	}
	
	@Override
	public String multipleRecordsToJsonArray(ByteArrayInputStream is) throws IOException {
		return cobolJsonConversion.multipleRecordsToJsonArray()
				.setInputCobolDataFile(is)
				.toJsonString();
	}
	
	@Override
	public List<AbstractLine> jsonArrayToMultipleRecords(String jsonString) throws IOException {
			return cobolJsonConversion.jsonArrayToMultipleRecords()
					.setJsonString(jsonString)
					.toCobolDataLines();
		}
}
