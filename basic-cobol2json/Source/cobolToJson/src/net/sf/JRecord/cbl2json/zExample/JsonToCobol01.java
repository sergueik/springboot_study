package net.sf.JRecord.cbl2json.zExample;

import java.io.IOException;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Cobol2Json;

public class JsonToCobol01 {

	public static void main(String[] args) throws IOException {
		Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("DTAR020.cbl"))
		  .setFont("cp037")
		  .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH) 
		  .setSplitCopybook(CopybookLoader.SPLIT_NONE)
		  
		  .jsonArrayToCobolFile("G:/Temp/DTAR020_tst1_normal.json", "G:/Temp/DTAR020_tst1_normal_json.bin")
		  
		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
		  .jsonArrayToCobolFile("G:/Temp/DTAR020_tst1.bin.json", "G:/Temp/DTAR020_tst1_bin_json.bin")
		  ;
	}

}
