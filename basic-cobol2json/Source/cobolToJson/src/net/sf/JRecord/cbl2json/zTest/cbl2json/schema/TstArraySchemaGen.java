package net.sf.JRecord.cbl2json.zTest.cbl2json.schema;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import org.junit.Test;

import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class TstArraySchemaGen {
	
	String copybook = ""
			+ "           03  Str-array occurs 9           pic x(20).\n"
			+ "           03  int-array occurs 5           pic s9(4).\n"
			+ "           03  num-array occurs 5           pic s9(4)v99.\n"
			+ "           03  occurs 5.\n"
			+ "               05 table-field  occurs 5     pic x(4). \n"
			+ "           03  table-1 occurs 5.\n"
			+ "               05 int-1 occurs 3            pic 9(9).\n"
			+ "           03  Group-array occurs 5.\n"
			+ "               05 int-2    occurs 3         pic 99.\n"
			+ "               05 group-2 occurs 5.  \n"
			+ "                  07 G-Field-3              pic x(3).\n"
			+ "                  07 num-3                  pic s9(4)V999.";
	
	String copybook2 = ""
			+ "        01 test-array.\n"
			+ copybook;

	
	private String STANDARD_ARRAY_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"object\",\n"
			+ "  \"properties\" : {\n"
			+ "    \"Array-Test\" : {\n"
			+ "      \"type\" : \"array\",\n"
			+ "      \"items\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"Str-array\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"string\"\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"int-array\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"integer\"\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"num-array\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"number\"\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"filler\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"object\",\n"
			+ "              \"properties\" : {\n"
			+ "                \"table-field\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"string\"\n"
			+ "                  }\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"table-1\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"object\",\n"
			+ "              \"properties\" : {\n"
			+ "                \"int-1\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"integer\"\n"
			+ "                  }\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"Group-array\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"object\",\n"
			+ "              \"properties\" : {\n"
			+ "                \"int-2\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"integer\"\n"
			+ "                  }\n"
			+ "                },\n"
			+ "                \"group-2\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"object\",\n"
			+ "                    \"properties\" : {\n"
			+ "                      \"G-Field-3\" : {\n"
			+ "                        \"type\" : \"string\"\n"
			+ "                      },\n"
			+ "                      \"num-3\" : {\n"
			+ "                        \"type\" : \"number\"\n"
			+ "                      }\n"
			+ "                    }\n"
			+ "                  }\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}"
			;
	
	private String ARRAY_ARRAY_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"array\",\n"
			+ "  \"items\" : {\n"
			+ "    \"type\" : \"object\",\n"
			+ "    \"properties\" : {\n"
			+ "      \"Str-array\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"string\"\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"int-array\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"integer\"\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"num-array\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"number\"\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"filler\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"object\",\n"
			+ "          \"properties\" : {\n"
			+ "            \"table-field\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"table-1\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"object\",\n"
			+ "          \"properties\" : {\n"
			+ "            \"int-1\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"Group-array\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"object\",\n"
			+ "          \"properties\" : {\n"
			+ "            \"int-2\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              }\n"
			+ "            },\n"
			+ "            \"group-2\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"object\",\n"
			+ "                \"properties\" : {\n"
			+ "                  \"G-Field-3\" : {\n"
			+ "                    \"type\" : \"string\"\n"
			+ "                  },\n"
			+ "                  \"num-3\" : {\n"
			+ "                    \"type\" : \"number\"\n"
			+ "                  }\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}";
	
	private String STANDARD_ARRAY_SCHEMA2 = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"object\",\n"
			+ "  \"properties\" : {\n"
			+ "    \"test-array\" : {\n"
			+ "      \"type\" : \"array\",\n"
			+ "      \"items\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"Str-array\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"string\"\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"int-array\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"integer\"\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"num-array\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"number\"\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"filler\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"object\",\n"
			+ "              \"properties\" : {\n"
			+ "                \"table-field\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"string\"\n"
			+ "                  }\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"table-1\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"object\",\n"
			+ "              \"properties\" : {\n"
			+ "                \"int-1\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"integer\"\n"
			+ "                  }\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"Group-array\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"object\",\n"
			+ "              \"properties\" : {\n"
			+ "                \"int-2\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"integer\"\n"
			+ "                  }\n"
			+ "                },\n"
			+ "                \"group-2\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"object\",\n"
			+ "                    \"properties\" : {\n"
			+ "                      \"G-Field-3\" : {\n"
			+ "                        \"type\" : \"string\"\n"
			+ "                      },\n"
			+ "                      \"num-3\" : {\n"
			+ "                        \"type\" : \"number\"\n"
			+ "                      }\n"
			+ "                    }\n"
			+ "                  }\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}"
			;
	
	private String ARRAY_ARRAY_SCHEMA2 = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"array\",\n"
			+ "  \"items\" : {\n"
			+ "    \"type\" : \"object\",\n"
			+ "    \"properties\" : {\n"
			+ "      \"Str-array\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"string\"\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"int-array\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"integer\"\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"num-array\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"number\"\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"filler\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"object\",\n"
			+ "          \"properties\" : {\n"
			+ "            \"table-field\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"table-1\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"object\",\n"
			+ "          \"properties\" : {\n"
			+ "            \"int-1\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"Group-array\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"object\",\n"
			+ "          \"properties\" : {\n"
			+ "            \"int-2\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              }\n"
			+ "            },\n"
			+ "            \"group-2\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"object\",\n"
			+ "                \"properties\" : {\n"
			+ "                  \"G-Field-3\" : {\n"
			+ "                    \"type\" : \"string\"\n"
			+ "                  },\n"
			+ "                  \"num-3\" : {\n"
			+ "                    \"type\" : \"number\"\n"
			+ "                  }\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}";


	@Test
	public void testStandard() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder(copybook);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		assertEquals(STANDARD_ARRAY_SCHEMA, schemaWriter.toString()); 
	}


	@Test
	public void testArray() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder(copybook)
				.setNameMainArray(false);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		
		assertEquals(ARRAY_ARRAY_SCHEMA, schemaWriter.toString()); 
		 
	}
	
	@Test
	public void testStandard2() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder(copybook2);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		assertEquals(STANDARD_ARRAY_SCHEMA2, schemaWriter.toString()); 
	}


	@Test
	public void testArray2() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder(copybook2)
				.setNameMainArray(false);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		
		assertEquals(ARRAY_ARRAY_SCHEMA2, schemaWriter.toString()); 
	}
		 

	
	private ICobol2Json getCobol2JsonNormalBuilder(String cobolCopybook) {
		JRecordConstantVars constants = Cobol2Json.JR_CONSTANTS;
	 	
	       return Cobol2Json.newCobol2Json(new StringReader(cobolCopybook), "Array-Test")

	                                         // Cobol Options
	                         .setFileOrganization(constants.IO_STANDARD_TEXT_FILE)
	                         .setDialect(constants.FMT_MAINFRAME)               
	                         .setSplitCopybook(constants.SPLIT_NONE)      
	                         .setFont("")
	                         .setPrettyPrint(true);

	}

}
