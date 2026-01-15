package net.sf.cobolToJson.zExample;

import java.io.ByteArrayOutputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import javax.xml.stream.XMLStreamException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.zTest.json2cbl.Cbl2JsonCode;
	
public class Dtar020ToJson02 {

    public static void main(String[] args)
    throws IOException,  XMLStreamException {
   
        JRecordConstantVars constants = Cobol2Json.JR_CONSTANTS;
        InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020.bin"));
        ByteArrayOutputStream output = new ByteArrayOutputStream();
	
        Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/DTAR020.cbl"))

                                         // Cobol Options
                         .setFileOrganization(constants.IO_FIXED_LENGTH)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setFont("cp037")
                         .setPrettyPrint(true)
                         .setNameMainArray(false)
                         .setWriteCheck("KCODE-STORE-KEY", new IWriteCheck() {
							@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
								int store = line.getFieldValue("STORE-NO").asInt();
								return store == 20;
							}
                          })
                         .setDropCopybookNameFromFields(true)

              .cobol2json(input, 
                          output);
        input.close();
        String JSON = new String(output.toByteArray());
		System.out.print(JSON);
    }
}
