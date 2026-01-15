package net.sf.cobolToJson.zTest.cbl2json.schema;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringWriter;

import org.junit.Test;

import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;
import net.sf.cobolToJson.zTest.json2cbl.Cbl2JsonCode;

public class TstDtar020SchemaGen1 {
	
	private String STANDARD_DTAR020_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"object\",\n"
			+ "  \"properties\" : {\n"
			+ "    \"DTAR020\" : {\n"
			+ "      \"type\" : \"array\",\n"
			+ "      \"items\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"DTAR020-KEYCODE-NO\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"DTAR020-STORE-NO\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"DTAR020-DATE\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"DTAR020-DEPT-NO\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"DTAR020-QTY-SOLD\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"DTAR020-SALE-PRICE\" : {\n"
			+ "            \"type\" : \"number\"\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}"
			;
	
	private String DTAR020_ARRAY_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"array\",\n"
			+ "  \"items\" : {\n"
			+ "    \"type\" : \"object\",\n"
			+ "    \"properties\" : {\n"
			+ "      \"DTAR020-KCODE-STORE-KEY\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"DTAR020-KEYCODE-NO\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"DTAR020-STORE-NO\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          }\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"DTAR020-DATE\" : {\n"
			+ "        \"type\" : \"integer\"\n"
			+ "      },\n"
			+ "      \"DTAR020-DEPT-NO\" : {\n"
			+ "        \"type\" : \"integer\"\n"
			+ "      },\n"
			+ "      \"DTAR020-QTY-SOLD\" : {\n"
			+ "        \"type\" : \"integer\"\n"
			+ "      },\n"
			+ "      \"DTAR020-SALE-PRICE\" : {\n"
			+ "        \"type\" : \"number\"\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}";

	private String DTAR020_FLATTEN_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"object\",\n"
			+ "  \"properties\" : {\n"
			+ "    \"DTAR020\" : {\n"
			+ "      \"type\" : \"array\",\n"
			+ "      \"items\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"DTAR020-KEYCODE-NO\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"DTAR020-STORE-NO\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"DTAR020-DATE\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"DTAR020-DEPT-NO\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"DTAR020-QTY-SOLD\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"DTAR020-SALE-PRICE\" : {\n"
			+ "            \"type\" : \"number\"\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}";

	@Test
	public void testStandard() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder();
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		assertEquals(STANDARD_DTAR020_SCHEMA, schemaWriter.toString()); 
	}

	@Test
	public void testFlatten() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder()	.setFlattenStructure(true);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		assertEquals(DTAR020_FLATTEN_SCHEMA, schemaWriter.toString()); 
	}


	@Test
	public void testArray() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder()
				.setNameMainArray(false);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		
		assertEquals(DTAR020_ARRAY_SCHEMA, schemaWriter.toString()); 
		 
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
