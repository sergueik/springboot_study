package net.sf.JRecord.cbl2json.zTest.cbl2json.schema;

import static org.junit.Assert.*;

import java.io.IOException;
import java.io.StringReader;
import java.io.StringWriter;

import org.junit.Test;

import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class TstDuplicateFieldFlatten {
	private static final String COPYBOOK = ""
			+ "       01  Record-1.\n"
			+ "         03 g1.\n"
			+ "              05 fld1           pic xx.\n"
			+ "              05 fld2           pic xx.\n"
			+ "              05 fld3           pic xx.\n"
			+ "         03 g2.\n"
			+ "              05 fld4           pic xx.\n"
			+ "              05 fld2           pic xx.\n"
			+ "          03 g3.\n"
			+ "              05 fld2           pic xx.\n"
			+ "              05 fld6           pic xx.\n"
			+ "          03 g4.\n"
			+ "              05 fld7           pic xx.\n"
			+ "              05 fld8           pic xx.";
	
	private static final String JSON_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"object\",\n"
			+ "  \"properties\" : {\n"
			+ "    \"Record-1\" : {\n"
			+ "      \"type\" : \"array\",\n"
			+ "      \"items\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"g1\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"fld1\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"fld2\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"fld3\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"g2\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"fld4\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"fld2\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"g3\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"fld2\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"fld6\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"fld7\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"fld8\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}";
	
	private static final String JSON_SAMPLE = ""
			+ "{\n"
			+ "  \"Record-1\" : [ {\n"
			+ "    \"g1\" : {\n"
			+ "      \"fld1\" : \"A\",\n"
			+ "      \"fld2\" : \"A\",\n"
			+ "      \"fld3\" : \"A\"\n"
			+ "    },\n"
			+ "    \"g2\" : {\n"
			+ "      \"fld4\" : \"A\",\n"
			+ "      \"fld2\" : \"A\"\n"
			+ "    },\n"
			+ "    \"g3\" : {\n"
			+ "      \"fld2\" : \"A\",\n"
			+ "      \"fld6\" : \"A\"\n"
			+ "    },\n"
			+ "    \"fld7\" : \"A\",\n"
			+ "    \"fld8\" : \"A\"\n"
			+ "  }, {\n"
			+ "    \"g1\" : { },\n"
			+ "    \"g2\" : { },\n"
			+ "    \"g3\" : { }\n"
			+ "  } ]\n"
			+ "}";

	
	
	@Test
	public void testFlatten() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder()	.setFlattenStructure(true);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		assertEquals(JSON_SCHEMA, schemaWriter.toString()); 
		
		StringWriter jsonWriter = new StringWriter();
		jsnBldr.writeSampleCobol2json(jsonWriter);
		assertEquals(JSON_SAMPLE, jsonWriter.toString()); 
	}

	private ICobol2Json getCobol2JsonNormalBuilder() {
		JRecordConstantVars constants = Cobol2Json.JR_CONSTANTS;
 	
       return Cobol2Json.newCobol2Json(new StringReader(COPYBOOK), "Record1")

                                         // Cobol Options
                         .setFileOrganization(constants.IO_FIXED_LENGTH)
                         .setDialect(constants.FMT_MAINFRAME)               
                         .setSplitCopybook(constants.SPLIT_NONE)      
                         .setPrettyPrint(true);

	}


}
