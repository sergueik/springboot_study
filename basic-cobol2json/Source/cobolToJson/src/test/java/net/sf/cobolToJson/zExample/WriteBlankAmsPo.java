package net.sf.cobolToJson.zExample;

import java.io.IOException;
import java.io.StringWriter;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.zTest.json2cbl.Cbl2JsonCode;

public class WriteBlankAmsPo {

	public static void main(String[] args) throws IOException {
		StringWriter w = new StringWriter();
		
		Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/amsPoDownload.cbl"))
		  .setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
		  .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
		  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
		  .writeSampleCobol2json(w);
		
		
		System.out.print(w.toString());
	}

}
