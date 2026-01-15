package net.sf.cobolToJson.zTest.cobolJsonConversion;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;
import java.util.List;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.cb2xml.copybookReader.CobolCopybookSource;
import net.sf.cb2xml.copybookReader.ReadCobolCopybook;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.CobolJsonConversion;
import net.sf.cobolToJson.def.ICobol2Json;
import net.sf.cobolToJson.def.ICobolJsonConversion;

public class Cobol2JsonDetails implements ICobolJsonConversionDetails  {

	
//	private final ICobolRecordToJsonObjectIn cobolToJson;
//	private final IJsonObjectToCobolRecordIn jsonToCobol;
	private final ICobol2Json cobolJsonConversion;
	public final IArrayCopybook cobolCopybookDetails;
	
	public Cobol2JsonDetails(IArrayCopybook cobolCopybookDetails) throws IOException {
		this.cobolJsonConversion = Cobol2Json.newCobol2Json(
							new CobolCopybookSource("Test-Copybook", cobolCopybookDetails.getCopybookString()))
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
		 return cobolJsonConversion.jsonObjectToCobolLine(new StringReader(jsonString));
	}

	@Override
	public String singleRecordToJsonObject(AbstractLine line) {
		try {
			return cobolJsonConversion.singleCobolRecord2jsonString(line.getData());
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}
	
	@Override
	public String multipleRecordsToJsonArray(ByteArrayInputStream is) throws IOException {
		StringWriter w = new StringWriter();
		cobolJsonConversion.cobol2json(is, w);
		return w.toString();
	}
	
	@Override
	public List<AbstractLine> jsonArrayToMultipleRecords(String jsonString) throws IOException {
		return cobolJsonConversion.jsonArrayToCobolLines(new StringReader(jsonString));
	}
}
