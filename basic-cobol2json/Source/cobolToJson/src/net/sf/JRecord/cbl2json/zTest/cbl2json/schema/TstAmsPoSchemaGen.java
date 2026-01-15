package net.sf.JRecord.cbl2json.zTest.cbl2json.schema;

import static org.junit.Assert.assertEquals;

import java.io.IOException;
import java.io.StringWriter;

import org.junit.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.JRecord.cbl2json.zTest.json2cbl.Cbl2JsonCode;
import net.sf.cobolToJson.Cobol2Json;
import net.sf.cobolToJson.def.ICobol2Json;

public class TstAmsPoSchemaGen {
	
	

	
	private String STANDARD_AMS_PO_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"object\",\n"
			+ "  \"properties\" : {\n"
			+ "    \"amsPoDownload\" : {\n"
			+ "      \"type\" : \"array\",\n"
			+ "      \"items\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"PO_Record\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"Record_Type\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Sequence_Number\" : {\n"
			+ "                \"type\" : \"number\"\n"
			+ "              },\n"
			+ "              \"Vendor\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              },\n"
			+ "              \"PO\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              },\n"
			+ "              \"Entry_Date\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"beg01_code\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"beg02_code\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Department\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Expected_Reciept_Date\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Cancel_by_date\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"EDI_Type\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Add_Date\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Department_Name\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Prcoess_Type\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Order_Type\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"Product_Record\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"Record_Type\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Pack_Qty\" : {\n"
			+ "                \"type\" : \"number\"\n"
			+ "              },\n"
			+ "              \"Pack_Cost\" : {\n"
			+ "                \"type\" : \"number\"\n"
			+ "              },\n"
			+ "              \"APN\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              },\n"
			+ "              \"Product\" : {\n"
			+ "                \"type\" : \"integer\"\n"
			+ "              },\n"
			+ "              \"pmg_dtl_tech_key\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Case_Pack_id\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"Product_Name\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              }\n"
			+ "            }\n"
			+ "          },\n"
			+ "          \"Location_Record\" : {\n"
			+ "            \"type\" : \"object\",\n"
			+ "            \"properties\" : {\n"
			+ "              \"Record_Type\" : {\n"
			+ "                \"type\" : \"string\"\n"
			+ "              },\n"
			+ "              \"location\" : {\n"
			+ "                \"type\" : \"array\",\n"
			+ "                \"items\" : {\n"
			+ "                  \"type\" : \"object\",\n"
			+ "                  \"properties\" : {\n"
			+ "                    \"DC_Number\" : {\n"
			+ "                      \"type\" : \"integer\"\n"
			+ "                    },\n"
			+ "                    \"Pack_Quantity\" : {\n"
			+ "                      \"type\" : \"integer\"\n"
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
	
	private String ARRAY_AMS_PO_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"array\",\n"
			+ "  \"items\" : {\n"
			+ "    \"type\" : \"object\",\n"
			+ "    \"properties\" : {\n"
			+ "      \"PO_Record\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"Record_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Sequence_Number\" : {\n"
			+ "            \"type\" : \"number\"\n"
			+ "          },\n"
			+ "          \"Vendor\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"PO\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"Entry_Date\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"beg01_code\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"beg02_code\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Department\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Expected_Reciept_Date\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Cancel_by_date\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"EDI_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Add_Date\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Department_Name\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Prcoess_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Order_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          }\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"Product_Record\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"Record_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Pack_Qty\" : {\n"
			+ "            \"type\" : \"number\"\n"
			+ "          },\n"
			+ "          \"Pack_Cost\" : {\n"
			+ "            \"type\" : \"number\"\n"
			+ "          },\n"
			+ "          \"APN\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"Product\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"pmg_dtl_tech_key\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Case_Pack_id\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Product_Name\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          }\n"
			+ "        }\n"
			+ "      },\n"
			+ "      \"Location_Record\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"Record_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"location\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"object\",\n"
			+ "              \"properties\" : {\n"
			+ "                \"DC_Number\" : {\n"
			+ "                  \"type\" : \"integer\"\n"
			+ "                },\n"
			+ "                \"Pack_Quantity\" : {\n"
			+ "                  \"type\" : \"integer\"\n"
			+ "                }\n"
			+ "              }\n"
			+ "            }\n"
			+ "          }\n"
			+ "        }\n"
			+ "      }\n"
			+ "    }\n"
			+ "  }\n"
			+ "}";
	
	private String TREE_AMS_PO_SCHEMA = ""
			+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"object\",\n"
			+ "  \"properties\" : {\n"
			+ "    \"PO_Record\" : {\n"
			+ "      \"type\" : \"array\",\n"
			+ "      \"items\" : {\n"
			+ "        \"type\" : \"object\",\n"
			+ "        \"properties\" : {\n"
			+ "          \"Record_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Sequence_Number\" : {\n"
			+ "            \"type\" : \"number\"\n"
			+ "          },\n"
			+ "          \"Vendor\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"PO\" : {\n"
			+ "            \"type\" : \"integer\"\n"
			+ "          },\n"
			+ "          \"Entry_Date\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"beg01_code\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"beg02_code\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Department\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Expected_Reciept_Date\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Cancel_by_date\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"EDI_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Add_Date\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Department_Name\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Prcoess_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Order_Type\" : {\n"
			+ "            \"type\" : \"string\"\n"
			+ "          },\n"
			+ "          \"Product_Record\" : {\n"
			+ "            \"type\" : \"array\",\n"
			+ "            \"items\" : {\n"
			+ "              \"type\" : \"object\",\n"
			+ "              \"properties\" : {\n"
			+ "                \"Record_Type\" : {\n"
			+ "                  \"type\" : \"string\"\n"
			+ "                },\n"
			+ "                \"Pack_Qty\" : {\n"
			+ "                  \"type\" : \"number\"\n"
			+ "                },\n"
			+ "                \"Pack_Cost\" : {\n"
			+ "                  \"type\" : \"number\"\n"
			+ "                },\n"
			+ "                \"APN\" : {\n"
			+ "                  \"type\" : \"integer\"\n"
			+ "                },\n"
			+ "                \"Product\" : {\n"
			+ "                  \"type\" : \"integer\"\n"
			+ "                },\n"
			+ "                \"pmg_dtl_tech_key\" : {\n"
			+ "                  \"type\" : \"string\"\n"
			+ "                },\n"
			+ "                \"Case_Pack_id\" : {\n"
			+ "                  \"type\" : \"string\"\n"
			+ "                },\n"
			+ "                \"Product_Name\" : {\n"
			+ "                  \"type\" : \"string\"\n"
			+ "                },\n"
			+ "                \"Location_Record\" : {\n"
			+ "                  \"type\" : \"array\",\n"
			+ "                  \"items\" : {\n"
			+ "                    \"type\" : \"object\",\n"
			+ "                    \"properties\" : {\n"
			+ "                      \"Record_Type\" : {\n"
			+ "                        \"type\" : \"string\"\n"
			+ "                      },\n"
			+ "                      \"location\" : {\n"
			+ "                        \"type\" : \"array\",\n"
			+ "                        \"items\" : {\n"
			+ "                          \"type\" : \"object\",\n"
			+ "                          \"properties\" : {\n"
			+ "                            \"DC_Number\" : {\n"
			+ "                              \"type\" : \"integer\"\n"
			+ "                            },\n"
			+ "                            \"Pack_Quantity\" : {\n"
			+ "                              \"type\" : \"integer\"\n"
			+ "                            }\n"
			+ "                          }\n"
			+ "                        }\n"
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
			+ "}";
			
	private String ARRAY_TREE_AMS_PO_SCHEMA = ""
					+ "{\n"
			+ "  \"$schema\" : \"http://json-schema.org/draft-06/schema#\",\n"
			+ "  \"type\" : \"array\",\n"
			+ "  \"items\" : {\n"
			+ "    \"type\" : \"object\",\n"
			+ "    \"properties\" : {\n"
			+ "      \"Record_Type\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Sequence_Number\" : {\n"
			+ "        \"type\" : \"number\"\n"
			+ "      },\n"
			+ "      \"Vendor\" : {\n"
			+ "        \"type\" : \"integer\"\n"
			+ "      },\n"
			+ "      \"PO\" : {\n"
			+ "        \"type\" : \"integer\"\n"
			+ "      },\n"
			+ "      \"Entry_Date\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"beg01_code\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"beg02_code\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Department\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Expected_Reciept_Date\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Cancel_by_date\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"EDI_Type\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Add_Date\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ ""
			+ "      \"Department_Name\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Prcoess_Type\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Order_Type\" : {\n"
			+ "        \"type\" : \"string\"\n"
			+ "      },\n"
			+ "      \"Product_Record\" : {\n"
			+ "        \"type\" : \"array\",\n"
			+ "        \"items\" : {\n"
			+ "          \"type\" : \"object\",\n"
			+ "          \"properties\" : {\n"
			+ "            \"Record_Type\" : {\n"
			+ "              \"type\" : \"string\"\n"
			+ "            },\n"
			+ "            \"Pack_Qty\" : {\n"
			+ "              \"type\" : \"number\"\n"
			+ "            },\n"
			+ "            \"Pack_Cost\" : {\n"
			+ "              \"type\" : \"number\"\n"
			+ "            },\n"
			+ "            \"APN\" : {\n"
			+ "              \"type\" : \"integer\"\n"
			+ "            },\n"
			+ "            \"Product\" : {\n"
			+ "              \"type\" : \"integer\"\n"
			+ "            },\n"
			+ "            \"pmg_dtl_tech_key\" : {\n"
			+ "              \"type\" : \"string\"\n"
			+ "            },\n"
			+ "            \"Case_Pack_id\" : {\n"
			+ "              \"type\" : \"string\"\n"
			+ "            },\n"
			+ "            \"Product_Name\" : {\n"
			+ "              \"type\" : \"string\"\n"
			+ "            },\n"
			+ "            \"Location_Record\" : {\n"
			+ "              \"type\" : \"array\",\n"
			+ "              \"items\" : {\n"
			+ "                \"type\" : \"object\",\n"
			+ "                \"properties\" : {\n"
			+ "                  \"Record_Type\" : {\n"
			+ "                    \"type\" : \"string\"\n"
			+ "                  },\n"
			+ "                  \"location\" : {\n"
			+ "                    \"type\" : \"array\",\n"
			+ "                    \"items\" : {\n"
			+ "                      \"type\" : \"object\",\n"
			+ "                      \"properties\" : {\n"
			+ "                        \"DC_Number\" : {\n"
			+ "                          \"type\" : \"integer\"\n"
			+ "                        },\n"
			+ "                        \"Pack_Quantity\" : {\n"
			+ "                          \"type\" : \"integer\"\n"
			+ "                        }\n"
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
			+ "}";

	@Test
	public void testStandard() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder();
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		assertEquals(STANDARD_AMS_PO_SCHEMA, schemaWriter.toString()); 
	}

	
	@Test
	public void testFlatten() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder()	.setFlattenStructure(true);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		assertEquals(STANDARD_AMS_PO_SCHEMA, schemaWriter.toString()); 
	}

	
	@Test
	public void testFlattenGroup() throws IOException {
		ICobol2Json jsnBldr = getCobol2JsonGroupBuilder()	.setFlattenStructure(true);
		StringWriter schemaWriter = new StringWriter();
		
		jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		
		//System.out.println(schemaWriter.toString());
		String expected = Conversion.replace(STANDARD_AMS_PO_SCHEMA, 
				"\"amsPoDownload\" : {", "\"amsPoDownloadGroup\" : {").toString();
		assertEquals(expected, schemaWriter.toString()); 
	}


	@Test
	public void testArray() throws IOException {
		//System.out.println(ARRAY_AMS_PO_SCHEMA);
		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder()
				.setNameMainArray(false);
		StringWriter schemaWriter = new StringWriter();
		
		//jsnBldr.writeSampleCobol2json("/Volumes/BruceMacHD/Temp/amsPoDownload_sample.json");
		
		try {
			jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		} catch (IOException e) {
			schemaWriter.close();
			e.printStackTrace();
		}
		
		//System.out.println(schemaWriter.toString());
		
		assertEquals(ARRAY_AMS_PO_SCHEMA, schemaWriter.toString()); 
		 
	}
	

	@Test
	public void testArrayFlattenGroup() throws IOException {
		//System.out.println(ARRAY_AMS_PO_SCHEMA);
		ICobol2Json jsnBldr = getCobol2JsonGroupBuilder()	
				.setFlattenStructure(true)
				.setNameMainArray(false);
		StringWriter schemaWriter = new StringWriter();
		
		//jsnBldr.writeSampleCobol2json("/Volumes/BruceMacHD/Temp/amsPoDownload_sample.json");
		
		try {
			jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		} catch (IOException e) {
			schemaWriter.close();
			e.printStackTrace();
		}
		
		//System.out.println(schemaWriter.toString());
		
		String expected = Conversion.replace(ARRAY_AMS_PO_SCHEMA, 
				"\"amsPoDownload\" : {", "\"amsPoDownloadGroup\" : {").toString();
		assertEquals(expected, schemaWriter.toString()); 
		 
	}


//	@Test
//	public void testTreeBldArray() throws IOException {
//		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder()
//				.setNameMainArray(false)
//				 .setRecordSelection("PO-Record", Cobol2Json.newFieldSelection("Record-Type","H1"))
//				 .setRecordSelection("Product-Record", Cobol2Json.newFieldSelection("Record-Type","D1"))
//				 .setRecordSelection("Location-Record", Cobol2Json.newFieldSelection("Record-Type","S1"))
//				.setRecordParent("Product-Record", "PO-Record")
//				.setRecordParent("Location-Record", "Product-Record")
//				.setPrettyPrint(true)
//				;
//
//		StringWriter jsonWriter = new StringWriter();
//		
//		jsnBldr.cobol2json(Cbl2JsonCode.getFullName("Ams_PODownload_20041231.txt"),
//				"/Volumes/BruceMacHD/Temp/amsPoDownload_tree3.json");
//		
//		ObjectMapper mapper = new ObjectMapper();
//		JsonNode sampleJson = mapper.readTree(new FileReader("/Volumes/BruceMacHD/Temp/amsPoDownload_tree3.json"));
//		JsonSchemaInferrer infer = JsonSchemaInferrer.newBuilder()
//			      .setSpecVersion(SpecVersion.DRAFT_06)
//			      .build();
//		
//		JsonNode schema = infer.inferForSample(sampleJson);
//		System.out.println(schema.toPrettyString());
//
//	}
	
//	@Test
//	public void testTreeBld() throws IOException {
//		ICobol2Json jsnBldr = getCobol2JsonNormalBuilder()
//				 .setRecordSelection("PO-Record", Cobol2Json.newFieldSelection("Record-Type","H1"))
//				 .setRecordSelection("Product-Record", Cobol2Json.newFieldSelection("Record-Type","D1"))
//				 .setRecordSelection("Location-Record", Cobol2Json.newFieldSelection("Record-Type","S1"))
//				.setRecordParent("Product-Record", "PO-Record")
//				.setRecordParent("Location-Record", "Product-Record")
//				.setPrettyPrint(true)
//				;
//
//		//StringWriter jsonWriter = new StringWriter();
//		
//		jsnBldr.cobol2json(Cbl2JsonCode.getFullName("Ams_PODownload_20041231.txt"),
//				"/Volumes/BruceMacHD/Temp/amsPoDownload_tree4.json");
//		
//		ObjectMapper mapper = new ObjectMapper();
//		JsonNode sampleJson = mapper.readTree(new FileReader("/Volumes/BruceMacHD/Temp/amsPoDownload_tree4.json"));
//		JsonSchemaInferrer infer = JsonSchemaInferrer.newBuilder()
//			      .setSpecVersion(SpecVersion.DRAFT_06)
//			      .build();
//		
//		JsonNode schema = infer.inferForSample(sampleJson);
//		System.out.println(schema.toPrettyString());
//
//	}

	

	@Test
	public void testArrayTree() throws IOException {
		ICobol2Json jsnBldr = createTreeBuilder()
				.setNameMainArray(false);
		StringWriter schemaWriter = new StringWriter();
		
		//jsnBldr.writeSampleCobol2json("/Volumes/BruceMacHD/Temp/amsPoDownload_sampleTree.json");
		
		try {
			jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		} catch (IOException e) {
			schemaWriter.close();
			e.printStackTrace();
		}
		
		//System.out.println(schemaWriter.toString());
		
		assertEquals(ARRAY_TREE_AMS_PO_SCHEMA, schemaWriter.toString()); 
		 
	}
//	@Test
//	public void testArrayTree2() throws IOException {
//		ICobol2Json jsnBldr = createTreeBuilder()
//				.setNameMainArray(false);
//		StringWriter schemaWriter = new StringWriter();
//			
//		//jsnBldr.writeSampleCobol2json("/Volumes/BruceMacHD/Temp/amsPoDownload_sampleTree.json");
//		
//		try {
//			jsnBldr.jsonSchemaForCobol2json2(schemaWriter);
//		} catch (IOException e) {
//			schemaWriter.close();
//			e.printStackTrace();
//		}
//		
//		//System.out.println(schemaWriter.toString());
//		
//		assertEquals(ARRAY_TREE_AMS_PO_SCHEMA, schemaWriter.toString()); 
//		 
//	}

	@Test
	public void testTree() throws IOException {
		ICobol2Json jsnBldr = createTreeBuilder()
				.setNameMainArray(true);
		StringWriter schemaWriter = new StringWriter();
			
		//jsnBldr.writeSampleCobol2json("/Volumes/BruceMacHD/Temp/amsPoDownload_sampleTree.json");
		
		try {
			jsnBldr.jsonSchemaForCobol2json(schemaWriter);
		} catch (IOException e) {
			schemaWriter.close();
			e.printStackTrace();
		}
		
		//System.out.println(schemaWriter.toString());
		
		assertEquals(TREE_AMS_PO_SCHEMA, schemaWriter.toString()); 
		 
	}

//	@Test
//	public void testTree2() throws IOException {
//		ICobol2Json jsnBldr = createTreeBuilder()
//				.setNameMainArray(true);
//		StringWriter schemaWriter = new StringWriter();
//			
//		//jsnBldr.writeSampleCobol2json("/Volumes/BruceMacHD/Temp/amsPoDownload_sampleTree.json");
//		
//		try {
//			jsnBldr.jsonSchemaForCobol2json2(schemaWriter);
//		} catch (IOException e) {
//			schemaWriter.close();
//			e.printStackTrace();
//		}
//		
//		//System.out.println(schemaWriter.toString());
//		
//		assertEquals(TREE_AMS_PO_SCHEMA, schemaWriter.toString()); 
//		 
//	}

	private ICobol2Json createTreeBuilder() {
		return getCobol2JsonNormalBuilder()
				.setRecordParent("Product-Record", "PO-Record")
				.setRecordParent("Location-Record", "Product-Record");
	}

	
	private ICobol2Json getCobol2JsonNormalBuilder() {
		return getCobol2JsonBldr("amsPoDownload.cbl");
	}
	
	private ICobol2Json getCobol2JsonGroupBuilder() {	 	
		return getCobol2JsonBldr("amsPoDownloadGroup.cbl");
	}

	
	private ICobol2Json getCobol2JsonBldr(String copybook) {
		 	
		return Cobol2Json.newCobol2Json(Cbl2JsonCode.getFullName("cobol/" + copybook))
					  .setFileOrganization(IFileStructureConstants.IO_BIN_TEXT)
	//				  .setPrettyPrint(true)
					  .setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
					  .setTagFormat(IReformatFieldNames.RO_UNDERSCORE);

	}

}
