package net.sf.JRecord.cbl2json.zTest.cbl2json.schema;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import org.junit.Test;

import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class TstRedefineSchemaGen {
	
	String copybook = ""
			+ "      01  Redefine-Test.\n"
			+ "           03  Group-Selector           pic x(01).\n"
			+ "           03  Group-1.\n"
			+ "               05 Field-1-1             pic x(25).\n"
			+ "           03  redefines Group-1.\n"
			+ "               05 NumField-2-1          pic s9(4). \n"
			+ "               05 NumField-2-2          pic s9(4)V99.\n"
			+ "               05 NumField-2-3          pic s9(4).\n"
			+ "           03  Group-3 redefines Group-1.\n"
			+ "               05  field-30-1           pic 9(9).";

	
	private String STANDARD_REDEFINE_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"object\",\n"
			+ "  \"properties\" : {\n"
			+ "    \"Redefine-Test\" : {\n"
			+ "      \"type\" : \"array\",\n"
			+ "      \"items\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"Group-Selector\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Group-1\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"Field-1-1\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"NumField-2-1\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"NumField-2-2\" : {\n"
			+ "            \"type\" : \"number\"\n"
			+ "          },\n"
			+ "          \"NumField-2-3\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"Group-3\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"field-30-1\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}"
			;
	
	private String ARRAY_REDEFINE_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"array\",\n"
			+ "  \"items\" : {\n"
			+ "    \"type\" : \"object\",\n"
			+ "    \"properties\" : {\n"
			+ "      \"Group-Selector\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Group-1\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"Field-1-1\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          }\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"NumField-2-1\" : {\n"
			+ "        \"type\" : \"integer\"\n"
			+ "      },\n"
			+ "      \"NumField-2-2\" : {\n"
			+ "        \"type\" : \"number\"\n"
			+ "      },\n"
			+ "      \"NumField-2-3\" : {\n"
			+ "        \"type\" : \"integer\"\n"
			+ "      },\n"
			+ "      \"Group-3\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"field-30-1\" : {\n"
			+ "            \"type\" : \"integer\"\n"
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
		assertEquals(STANDARD_REDEFINE_SCHEMA, schemaWriter.toString()); 
	}


	@Test
	public void testArray() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder()
				.setNameMainArray(false);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		
		assertEquals(ARRAY_REDEFINE_SCHEMA, schemaWriter.toString()); 
		 
	}
	
	private ICobol2Json getCobol2JsonNormalBuilder() {
		JRecordConstantVars constants = Cobol2Json.JR_CONSTANTS;
	 	
	       return Cobol2Json.newCobol2Json(new StringReader(copybook), "RedefineTst")

	                                         // Cobol Options
	                         .setFileOrganization(constants.IO_STANDARD_TEXT_FILE)
	                         .setDialect(constants.FMT_MAINFRAME)               
	                         .setSplitCopybook(constants.SPLIT_NONE)      
	                         .setFont("")
	                         .setPrettyPrint(true);

	}

}
