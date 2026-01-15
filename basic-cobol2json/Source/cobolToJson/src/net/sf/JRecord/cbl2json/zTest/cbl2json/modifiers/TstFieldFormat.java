package net.sf.JRecord.cbl2json.zTest.cbl2json.modifiers;

import static org.junit.Assert.assertEquals;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;

import org.junit.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.JRecord.schema.jaxb.impl.ZeroPad;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class TstFieldFormat {
	private static final String ZERO_PAD_JSON = ""
			+ "{\n"
			+ "  \"DTAR020\" : [ {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"69694158\",\n"
			+ "      \"DTAR020-STORE-NO\" : \"020\"\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : \"280\",\n"
			+ "    \"DTAR020-QTY-SOLD\" : \"000000001\",\n"
			+ "    \"DTAR020-SALE-PRICE\" : 5.01\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"63604808\",\n"
			+ "      \"DTAR020-STORE-NO\" : \"020\"\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : \"170\",\n"
			+ "    \"DTAR020-QTY-SOLD\" : \"000000001\",\n"
			+ "    \"DTAR020-SALE-PRICE\" : 4.87\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"68634752\",\n"
			+ "      \"DTAR020-STORE-NO\" : \"059\"\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : \"410\",\n"
			+ "    \"DTAR020-QTY-SOLD\" : \"000000001\",\n"
			+ "    \"DTAR020-SALE-PRICE\" : 8.99\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"64674965\",\n"
			+ "      \"DTAR020-STORE-NO\" : \"166\"\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : \"235\",\n"
			+ "    \"DTAR020-QTY-SOLD\" : \"-00000001\",\n"
			+ "    \"DTAR020-SALE-PRICE\" : -19.99\n"
			+ "  } ]\n"
			+ "}";
	
	@Test
	public void test1() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder();
		
		IFormatField zeroPad = newZeroPad();
		
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
	    StringWriter w = new StringWriter();

		try {
			jsnBldr
				.setFormatField("DTAR020-STORE-NO", zeroPad)
				.setFormatField("DTAR020-DEPT-NO", zeroPad)
				.setFormatField("DTAR020-QTY-SOLD", zeroPad)
				
				.cobol2json(input, w)
				;
		} catch (Throwable e) {
			// TODO Auto-generated catch block
			//e.printStackTrace();
			System.out.print(w.toString());
			throw e;
		}
		
//	    System.out.print(w.toString());
	    
	    assertEquals(ZERO_PAD_JSON, w.toString());
	}

	@Test
	public void test2() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonDropNamesBuilder();
		
		IFormatField zeroPad = newZeroPad();
		
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
	    StringWriter w = new StringWriter();

		jsnBldr
			.setFormatField("STORE-NO", zeroPad)
			.setFormatField("DEPT-NO", zeroPad)
			.setFormatField("QTY-SOLD", zeroPad)
			
			.cobol2json(input, w)
			;
		
//	    System.out.print(w.toString());
	    String expected = Conversion.replace(ZERO_PAD_JSON, "DTAR020-", "").toString();
	    

	    assertEquals(expected, w.toString());
	}


	private IFormatField newZeroPad() {
		return new ZeroPad();
	}

	
	private ICobol2Json getCobol2JsonDropNamesBuilder() {
	       return getCobol2JsonNormalBuilder()
	                          .setDropCopybookNameFromFields(true);
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

}
