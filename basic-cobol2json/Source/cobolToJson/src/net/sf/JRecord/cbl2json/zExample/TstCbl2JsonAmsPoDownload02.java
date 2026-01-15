package net.sf.JRecord.cbl2json.zExample;

import java.io.IOException;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Cobol2Json;

public class TstCbl2JsonAmsPoDownload02 {

	public static void main(String[] args) throws IOException {
		Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/amsPoDownload.cbl"))
		  .setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
		  .setPrettyPrint(true)
		  .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
		  
			 .setRecordSelection("PO-Record", Cobol2Json.newFieldSelection("Record-Type","H1"))
			 .setRecordSelection("Product-Record", Cobol2Json.newFieldSelection("Record-Type","D1"))
			 .setRecordSelection("Location-Record", Cobol2Json.newFieldSelection("Record-Type","S1"))
			 
		.cobol2json(Cbl2JsonCode.getFullName("Ams_PODownload_20041231_Invalid_RecordType.txt"), 
				"/Volumes/BruceMacHD/Temp/amsPoDownload_records_2.json");
	}

}
