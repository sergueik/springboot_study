package net.sf.JRecord.cbl2json.zExample;

import java.io.IOException;
import java.io.StringWriter;

import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class WriteBlankDtar020 {
	
	WriteBlankDtar020() throws IOException {
		StringWriter w = new StringWriter();
		
		getCobol2JsonNormalBuilder()
			.writeSampleCobol2json(w);
		
		System.out.print(w.toString());
	}

	private ICobol2Json getCobol2JsonNormalBuilder() {
		JRecordConstantVars constants = Cobol2Json.JR_CONSTANTS;
 	
       return Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/DTAR020.cbl"))

                                         // Cobol Options
                         .setFileOrganization(constants.IO_FIXED_LENGTH)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setFont("cp037")
                         .setPrettyPrint(true);

	}
	public static void main(String[] args) throws  IOException {
		new WriteBlankDtar020();
	}


}
