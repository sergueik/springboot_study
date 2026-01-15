package net.sf.cobolToJson.zTest.cbl2json.modifiers;

import static org.junit.Assert.*;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.math.BigDecimal;

import org.junit.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;
import net.sf.cobolToJson.zTest.json2cbl.Cbl2JsonCode;

public class TstIWriteCheck {

	private static final String STANDARD_JSON = ""
			+ "{\n"
			+ "  \"DTAR020\" : [ {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"69694158\",\n"
			+ "      \"DTAR020-STORE-NO\" : 20\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 280,\n"
			+ "    \"DTAR020-QTY-SOLD\" : 1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : 5.01\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"63604808\",\n"
			+ "      \"DTAR020-STORE-NO\" : 20\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 170,\n"
			+ "    \"DTAR020-QTY-SOLD\" : 1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : 4.87\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"68634752\",\n"
			+ "      \"DTAR020-STORE-NO\" : 59\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 410,\n"
			+ "    \"DTAR020-QTY-SOLD\" : 1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : 8.99\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"64674965\",\n"
			+ "      \"DTAR020-STORE-NO\" : 166\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 235,\n"
			+ "    \"DTAR020-QTY-SOLD\" : -1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : -19.99\n"
			+ "  } ]\n"
			+ "}";
	
	private static final String DROP_KEYCODE_STORE_JSON = ""
			+ "{\n"
			+ "  \"DTAR020\" : [ {\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 280,\n"
			+ "    \"DTAR020-QTY-SOLD\" : 1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : 5.01\n"
			+ "  }, {\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 170,\n"
			+ "    \"DTAR020-QTY-SOLD\" : 1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : 4.87\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"68634752\",\n"
			+ "      \"DTAR020-STORE-NO\" : 59\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 410,\n"
			+ "    \"DTAR020-QTY-SOLD\" : 1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : 8.99\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"64674965\",\n"
			+ "      \"DTAR020-STORE-NO\" : 166\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 235,\n"
			+ "    \"DTAR020-QTY-SOLD\" : -1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : -19.99\n"
			+ "  } ]\n"
			+ "}";
	
	private static final String DROP_KEYCODE_STORE_PRICE_JSON = ""
			+ "{\n"
			+ "  \"DTAR020\" : [ {\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 280,\n"
			+ "    \"DTAR020-QTY-SOLD\" : 1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : 5.01\n"
			+ "  }, {\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 170,\n"
			+ "    \"DTAR020-QTY-SOLD\" : 1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : 4.87\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"68634752\",\n"
			+ "      \"DTAR020-STORE-NO\" : 59\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 410\n"
			+ "  }, {\n"
			+ "    \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "      \"DTAR020-KEYCODE-NO\" : \"64674965\",\n"
			+ "      \"DTAR020-STORE-NO\" : 166\n"
			+ "    },\n"
			+ "    \"DTAR020-DATE\" : 40118,\n"
			+ "    \"DTAR020-DEPT-NO\" : 235,\n"
			+ "    \"DTAR020-QTY-SOLD\" : -1,\n"
			+ "    \"DTAR020-SALE-PRICE\" : -19.99\n"
			+ "  } ]\n"
			+ "}";
	
	@Test
	public void testNormal() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonNormalBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2json.cobol2json(input, w);
	    
//	    System.out.print(w.toString());
	    
	    assertEquals(STANDARD_JSON, w.toString());
	}
	
	
	@Test
	public void testNormalDropStore() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonNormalBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2json
	      .setWriteCheck("DTAR020-KCODE-STORE-KEY", newDTARKeycodeStore())
	    	.cobol2json(input, w);
	    
//	    System.out.print(w.toString());
	    
	    assertEquals(DROP_KEYCODE_STORE_JSON, w.toString());
	}

	
	@Test
	public void testNormalDropStorePrice() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonNormalBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    IWriteCheck priceCheck = new IWriteCheck() {
				@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
					BigDecimal price = line.getFieldValue("DTAR020-SALE-PRICE").asBigDecimal();
					return ! (new BigDecimal("8.99")) .equals(price);
				}
		    };
	    
	    cbl2json
	        .setWriteCheck("DTAR020-KCODE-STORE-KEY", newDTARKeycodeStore())
	    	.setWriteCheck("DTAR020-QTY-SOLD", priceCheck)
	    	.setWriteCheck("DTAR020-SALE-PRICE", priceCheck)

	    	.cobol2json(input, w);
	    
	    System.out.print(w.toString());
	    
	    assertEquals(DROP_KEYCODE_STORE_PRICE_JSON, w.toString());
	}

	private IWriteCheck newDTARKeycodeStore() {
		return new IWriteCheck() {
				@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
					int store = line.getFieldValue("DTAR020-STORE-NO").asInt();
					return store != 20;
				}
	       };
	}

	
	@Test
	public void testDropCopybook() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonDropNamesBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2json.cobol2json(input, w);
	    
//	    System.out.print(w.toString());
	    
	    String expected = Conversion.replace(STANDARD_JSON, "DTAR020-", "").toString();
	    
	    assertEquals(expected, w.toString());
	}

	
	@Test
	public void testDropCopybookAndStorePrice() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonDropNamesBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    IWriteCheck priceCheck =  createPriceCheck();
	    
	    cbl2json
	    	.setWriteCheck("KCODE-STORE-KEY", CreateStoreTest())
	    	.setWriteCheck("QTY-SOLD", priceCheck)
	    	.setWriteCheck("SALE-PRICE", priceCheck)
	    	.cobol2json(input, w);
	    
//	    System.out.print(w.toString());
	    
	    String expected = Conversion.replace(DROP_KEYCODE_STORE_PRICE_JSON, "DTAR020-", "").toString();
	    
	    assertEquals(expected, w.toString());
	}

	
	@Test
	public void testDropCopybookAndStore() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonDropNamesBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2json
	    	.setWriteCheck("KCODE-STORE-KEY", CreateStoreTest())
	    	.cobol2json(input, w);
	    
//	    System.out.print(w.toString());
	    
	    String expected = Conversion.replace(DROP_KEYCODE_STORE_JSON, "DTAR020-", "").toString();
	    
	    assertEquals(expected, w.toString());
	}

	@Test
	public void testNoMainArray() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonNoMainArrayBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2json.cobol2json(input, w);
	    
	   // System.out.print(w.toString());
	    
	    String expected = ""
	    		+ "[ {\n"
	    		+ "  \"KCODE-STORE-KEY\" : {\n"
	    		+ "    \"KEYCODE-NO\" : \"69694158\",\n"
	    		+ "    \"STORE-NO\" : 20\n"
	    		+ "  },\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 280,\n"
	    		+ "  \"QTY-SOLD\" : 1,\n"
	    		+ "  \"SALE-PRICE\" : 5.01\n"
	    		+ "}, {\n"
	    		+ "  \"KCODE-STORE-KEY\" : {\n"
	    		+ "    \"KEYCODE-NO\" : \"63604808\",\n"
	    		+ "    \"STORE-NO\" : 20\n"
	    		+ "  },\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 170,\n"
	    		+ "  \"QTY-SOLD\" : 1,\n"
	    		+ "  \"SALE-PRICE\" : 4.87\n"
	    		+ "}, {\n"
	    		+ "  \"KCODE-STORE-KEY\" : {\n"
	    		+ "    \"KEYCODE-NO\" : \"68634752\",\n"
	    		+ "    \"STORE-NO\" : 59\n"
	    		+ "  },\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 410,\n"
	    		+ "  \"QTY-SOLD\" : 1,\n"
	    		+ "  \"SALE-PRICE\" : 8.99\n"
	    		+ "}, {\n"
	    		+ "  \"KCODE-STORE-KEY\" : {\n"
	    		+ "    \"KEYCODE-NO\" : \"64674965\",\n"
	    		+ "    \"STORE-NO\" : 166\n"
	    		+ "  },\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 235,\n"
	    		+ "  \"QTY-SOLD\" : -1,\n"
	    		+ "  \"SALE-PRICE\" : -19.99\n"
	    		+ "} ]";
	    
	    assertEquals(expected, w.toString());
	}
	
	@Test
	public void testDropOnStore() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonNoMainArrayBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    cbl2json
	      .setWriteCheck("KCODE-STORE-KEY", CreateStoreTest())

	    	.cobol2json(input, w);
	    
	   // System.out.print(w.toString());
	    
	    String expected = ""
	    		+ "[ {\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 280,\n"
	    		+ "  \"QTY-SOLD\" : 1,\n"
	    		+ "  \"SALE-PRICE\" : 5.01\n"
	    		+ "}, {\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 170,\n"
	    		+ "  \"QTY-SOLD\" : 1,\n"
	    		+ "  \"SALE-PRICE\" : 4.87\n"
	    		+ "}, {\n"
	    		+ "  \"KCODE-STORE-KEY\" : {\n"
	    		+ "    \"KEYCODE-NO\" : \"68634752\",\n"
	    		+ "    \"STORE-NO\" : 59\n"
	    		+ "  },\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 410,\n"
	    		+ "  \"QTY-SOLD\" : 1,\n"
	    		+ "  \"SALE-PRICE\" : 8.99\n"
	    		+ "}, {\n"
	    		+ "  \"KCODE-STORE-KEY\" : {\n"
	    		+ "    \"KEYCODE-NO\" : \"64674965\",\n"
	    		+ "    \"STORE-NO\" : 166\n"
	    		+ "  },\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 235,\n"
	    		+ "  \"QTY-SOLD\" : -1,\n"
	    		+ "  \"SALE-PRICE\" : -19.99\n"
	    		+ "} ]";
	    
	    assertEquals(expected, w.toString());
	}


	private IWriteCheck CreateStoreTest() {
		IWriteCheck storeCheck = new IWriteCheck() {
				@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
					int store = line.getFieldValue("STORE-NO").asInt();
					return store != 20;
				}
	       };
		return storeCheck;
	}

	@Test
	public void testDropOnStoreAndPrice() throws IOException {
		ICobol2Json cbl2json = getCobol2JsonNoMainArrayBuilder();
	    InputStream input = new FileInputStream(Cbl2JsonCode.getFullName("DTAR020_4rows.bin"));
       // ByteArrayOutputStream output = new ByteArrayOutputStream();
	    StringWriter w = new StringWriter();
	    
	    IWriteCheck priceCheck =  createPriceCheck();
	    
	    cbl2json
	    	.setWriteCheck("KCODE-STORE-KEY", CreateStoreTest())
	    	.setWriteCheck("QTY-SOLD", priceCheck)
	    	.setWriteCheck("SALE-PRICE", priceCheck)

	    	.cobol2json(input, w);
	    
	   // System.out.print(w.toString());
	    
	    String expected = ""
	    		+ "[ {\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 280,\n"
	    		+ "  \"QTY-SOLD\" : 1,\n"
	    		+ "  \"SALE-PRICE\" : 5.01\n"
	    		+ "}, {\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 170,\n"
	    		+ "  \"QTY-SOLD\" : 1,\n"
	    		+ "  \"SALE-PRICE\" : 4.87\n"
	    		+ "}, {\n"
	    		+ "  \"KCODE-STORE-KEY\" : {\n"
	    		+ "    \"KEYCODE-NO\" : \"68634752\",\n"
	    		+ "    \"STORE-NO\" : 59\n"
	    		+ "  },\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 410\n"
	    		+ "}, {\n"
	    		+ "  \"KCODE-STORE-KEY\" : {\n"
	    		+ "    \"KEYCODE-NO\" : \"64674965\",\n"
	    		+ "    \"STORE-NO\" : 166\n"
	    		+ "  },\n"
	    		+ "  \"DATE\" : 40118,\n"
	    		+ "  \"DEPT-NO\" : 235,\n"
	    		+ "  \"QTY-SOLD\" : -1,\n"
	    		+ "  \"SALE-PRICE\" : -19.99\n"
	    		+ "} ]";
	    
	    assertEquals(expected, w.toString());
	}


	private IWriteCheck createPriceCheck() {
		return new IWriteCheck() {
			@Override public boolean isOkToWrite(IItem item, AbstractLine line) {
				BigDecimal price = line.getFieldValue("SALE-PRICE").asBigDecimal();
				return ! (new BigDecimal("8.99")) .equals(price);
			}
	    };
	}


	private ICobol2Json getCobol2JsonNoMainArrayBuilder() {
       return getCobol2JsonNormalBuilder()
                         .setNameMainArray(false)
                         .setDropCopybookNameFromFields(true);
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

