
package net.sf.cobolToJson.impl;

import java.io.BufferedOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.io.Writer;
import java.math.BigDecimal;
import java.util.List;

//import com.fasterxml.jackson.core.JsonParseException;
//import com.fasterxml.jackson.databind.JsonNode;
//import com.fasterxml.jackson.databind.ObjectMapper;
//import com.saasquatch.jsonschemainferrer.JsonSchemaInferrer;
//import com.saasquatch.jsonschemainferrer.SpecVersion;

import net.sf.JRecord.Common.AbstractFieldValue;
import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.External.CobolCopybookLoader;
import net.sf.JRecord.External.ICopybookLoaderCobol;
import net.sf.JRecord.External.XmlCopybookLoader;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.IReadLine;
import net.sf.JRecord.IO.ListLineWriter;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.schema.CobolSchemaDetails;
import net.sf.JRecord.schema.CobolSchemaReader;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.ISchemaInformation;
import net.sf.JRecord.schema.jaxb.IItem;
import net.sf.JRecord.schema.jaxb.Item;
import net.sf.JRecord.schema.jaxb.ItemRecordDtls;
import net.sf.JRecord.schema.jaxb.LineItemHelper;
import net.sf.JRecord.util.errorLog.BasicErrorLog;
import net.sf.JRecord.util.errorLog.ILogLinesInError;
//import net.sf.JRecord.schema.jaxb.Item;
import net.sf.cobolToJson.def.ICobol2Json;
import net.sf.cobolToJson.def.Icb2xml2Json;
import net.sf.cobolToJson.impl.jsonSchema.WriteJsonSchema;
import net.sf.cobolToJson.impl.jsonWriter.IJsonWriter;
import net.sf.cobolToJson.impl.jsonWriter.JsonSchemaCreator;
import net.sf.cobolToJson.impl.jsonWriter.JsonWriter;
import net.sf.cobolToJson.impl.readJson.JsonToCobol;
import net.sf.cobolToJson.impl.readJson.ToJRecordFile;
import net.sf.cobolToJson.impl.readJson.ToJRecordLine;
import net.sf.cobolToJson.impl.updateSchema.BlankLineReader;
import net.sf.cobolToJson.impl.updateSchema.BlankLineReaderAllOptions;
import net.sf.cobolToJson.impl.updateSchema.UpdateDetailsForSampleRecords;


/**
 * Purpose: Convert Cobol-Data-Files <---> Xml files
 *  
 * @author Bruce Martin
 *
 */
public class Cobol2JsonImp extends CobolSchemaReader<ICobol2Json> implements ICobol2Json {

	
	private ISchemaInformation itemDtls = null;
	
	private CobolSchemaDetails cobolSchemaDetails = null;

	private boolean skipValidation, prettyPrint = true, nameMainArray = true;
	
	private ILogLinesInError logErrors = new BasicErrorLog();

	

	
	
	private Cobol2JsonImp(String copybookFilename, ICopybookLoaderCobol loader) {
		super(Conversion.getCopyBookId(copybookFilename), loader);
		loader.setSaveCb2xmlDocument(true);
		super.addCopyBook(copybookFilename);
		//ioBuilder = new CblIOBuilderMultiSchema(copybookFilename, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2JsonImp(InputStream is, String copybookname, ICopybookLoaderCobol loader) {
		super(copybookname, loader);
		loader.setSaveCb2xmlDocument(true);
		
		super.addCopyBook(is, copybookname);
		//ioBuilder = new CblIOBuilderMultiSchema(is, copybookname, loader, ICopybookDialects.FMT_MAINFRAME);
	}

	private Cobol2JsonImp(Reader copybookReader, String copybookname, ICopybookLoaderCobol loader) {
		super(copybookname, loader);
		loader.setSaveCb2xmlDocument(true);
		
		super.addCopyBook(copybookReader, copybookname);
	}


	
	@Override
	public void cobol2json(String cobolFileName, String jsonFileName) throws IOException  {
		cobol2json(new FileInputStream(cobolFileName), new BufferedOutputStream(new FileOutputStream(jsonFileName), 0x4000));
	}
	
	@Override
	public void cobol2json(InputStream cobolStream, Writer writer) throws IOException {
		doInit();
		
        cobol2json(cobolSchemaDetails.ioBuilder.newReader(cobolStream), newJsonGenerator(writer));
        writer.close();
	}

	

	@Override
	public String singleCobolRecord2jsonString(byte[] cobolData) throws IOException {
		StringWriter w = new StringWriter(cobolData.length * 8);
		singleCobolRecord2json(cobolData, w);
		return w.toString();
	}

	

	@Override
	public void singleCobolRecord2json(byte[] cobolData, Writer writer) throws IOException {
		doInit();
		
		JsonWriter w = newJsonGenerator(writer);
		LineItemHelper lineItemHelper = new LineItemHelper(cobolSchemaDetails.schema, false);

		w.initWriter(prettyPrint);
//        if (prettyPrint) {
//        	w.setPrettyPrinter(new DefaultPrettyPrinter());
//        }
    	//w.writeStartObject();
		writeLineAsRecord(
				w, 
				cobolSchemaDetails.ioBuilder
					.newLine(cobolData), 
				lineItemHelper, 
				cobolSchemaDetails.recordItems, 
				0);
		//w.writeEndObject();
		w.close();
		
		logErrors.reportErrors();

//        cobol2json(
//        		new SingleLineReader(
//        				cobolSchemaDetails.ioBuilder
//        					.newLine(cobolData)),
//        		new JsonFactory().createGenerator(writer));
	}

	
	
	@Override
	public void cobol2json(InputStream cobolStream, OutputStream jsonStream) throws IOException {
		doInit();
		
        cobol2json(cobolSchemaDetails.ioBuilder.newReader(cobolStream), new JsonWriter(jsonStream));
        jsonStream.close();
	}
	
	@Override
	public ICobol2Json writeSampleCobol2json(String fileName) throws IOException {
		return writeSampleCobol2json(new FileWriter(fileName));
	}
	
	@Override
	public ICobol2Json writeSampleCobol2json(Writer writer) throws IOException {
		UpdateDetailsForSampleRecords updDtls = doInitForSampleJson(true);
		
		LayoutDetail layout = getLayout();
		cobol2json(layout, new BlankLineReaderAllOptions(updDtls.getExitManager(), layout), newJsonGenerator(writer), false);
		return this;
	}
	
	@Override
	public ICobol2Json jsonSchemaForCobol2json(Writer writer) throws IOException {
		doInitForSampleJson(true);
		LayoutDetail layout = getLayout();
		JsonSchemaCreator schemaCreator = new JsonSchemaCreator();

		cobol2json(layout, new BlankLineReader(layout), schemaCreator, true);
		
		new WriteJsonSchema(writer, schemaCreator.getSchemaItem());

		return this;
	}
	
	
//	public ICobol2Json jsonSchemaForCobol2json2(Writer writer) throws IOException {
//		StringWriter sampleJsonWriter = new StringWriter();
//		
//		writeSampleCobol2json(sampleJsonWriter);
//		
//		ObjectMapper mapper = new ObjectMapper();
//		JsonNode sampleJson = mapper.readTree(new StringReader(sampleJsonWriter.toString()));
//		JsonSchemaInferrer infer = JsonSchemaInferrer.newBuilder()
//			      .setSpecVersion(SpecVersion.DRAFT_06)
//			      .build();
//		
//		JsonNode schema = infer.inferForSample(sampleJson);
//		writer.write(schema.toPrettyString());
//		writer.close();
//
//		
//		return this;
//	}


	private JsonWriter newJsonGenerator(Writer writer) throws IOException {
		return new JsonWriter(writer);
	}
	
	private void cobol2json(AbstractLineReader reader, JsonWriter writer) throws IOException {
		cobol2json(reader.getLayout(), reader, writer, false);
		
		reader.close();
	}


	private void cobol2json(LayoutDetail schema, IReadLine reader, IJsonWriter jsonWriter, boolean arraySizeOne) throws IOException {
//		LayoutDetail schema =  reader.getLayout();
//	private void cobol2json(AbstractLineReader reader, JsonGenerator writer) throws IOException {
//		LayoutDetail schema =  reader.getLayout();
        AbstractLine l;
        LineItemHelper lineItemHelper = new LineItemHelper(schema, arraySizeOne);
        //List<? extends IItem> items = cobolSchemaDetails.cobolCopybook.getCobolItems();
       	List<ItemRecordDtls> recordItems = cobolSchemaDetails.recordItems;

       	//writer.set
//       	jsonWriter.enable(JsonGenerator.Feature.AUTO_CLOSE_JSON_CONTENT);
//       	jsonWriter.enable(JsonGenerator.Feature.FLUSH_PASSED_TO_STREAM);
//        if (prettyPrint) {
//        	jsonWriter.setPrettyPrinter(new DefaultPrettyPrinter());
//        }
       	
       	jsonWriter.initWriter(prettyPrint);
        
 		try {
	    	//writer.writeStartObject();
			if (recordItems.size() == 1) {
		    	//ItemRecordDtls itemRecordDtls = recordItems.get(0);
		    	List<Item> itemList = recordItems.get(0).items;
		    	Item itm = itemList.size() == 0 ? null : itemList.get(0) ;
				String recordName = schema.getRecord(0).getRecordName();
				if (itemList.size() == 1 
				&& (itm).getItemType() == Item.TYPE_GROUP
				&& itm.getChildItems().size() > 0) {
					if (itm.getName() != null && itm.getName().length() > 0) {
						recordName = itm.getName();
					}
					itemList = itm.getChildItems();
				}
				
				arrayStart(itm, jsonWriter, itemDtls.updateName(recordName));
				//writer.writeArrayFieldStart(itemDtls.updateName(recordName));
			    while ((l = reader.read()) != null) {
			    	jsonWriter.writeStartObject();
					writeItems(jsonWriter, lineItemHelper.setLine(l), itemList, new IntStack());
			        jsonWriter.writeEndObject();
			    }
			} else if (schema.hasTreeStructure()) {
			   	ReadManager rm = new ReadManager(reader, schema, lineItemHelper);
				String rootRecordName = super.getRootRecord();
			   	rm.read();
			   	
			   	int rootIdx = -1;
				if (rootRecordName == null) {
					for (int i = 0; i < schema.getRecordCount(); i++) {
						if (schema.getRecord(i).getParentRecordIndex() < 0) {
							rootRecordName = schema.getRecord(i).getRecordName();
							rootIdx = i;
							break;
						}
					}
				} else {
					rootIdx = schema.getRecordIndex(rootRecordName);
				}
					
				if (rootRecordName == null) {
				   	//writer.writeStartArray();
					arrayStart(null, jsonWriter, 
							cobolSchemaDetails.copybookInformation.updateName(cobolSchemaDetails.schema.getLayoutName()));
					//writer.writeArrayFieldStart(cobolSchemaDetails.copybookInformation.updateName(cobolSchemaDetails.schema.getLayoutName()));
					while (rm.line != null) {
						jsonWriter.writeStartObject();
						writeItemInTree(jsonWriter, rm, recordItems);
						jsonWriter.writeEndObject();
					}
				} else {
					if (rootIdx < 0) { throw new RecordException("Root Record: " + rootRecordName + " does not exist" ); }
					RecordDetail rec = schema.getRecord(rootIdx);
					if (rec.getParentRecordIndex() >= 0) {new RecordException("Root Record: " + rootRecordName + " has a parent record ???" ); }
					
					
					for (int i =0; i < schema.getRecordCount(); i++) {
						if (i != rootIdx && schema.getRecord(i).getParentRecordIndex() < 0) {
							throw new RecordException("Schema has more than one root: " + schema.getRecord(i).getRecordName());
						}
					}
					if (rm.recordIdx != rootIdx) {
						if (rm.recordIdx < 0) {
							throw new RecordException("Can not determine the record type for the first record");
						}
						throw new RecordException("Invalid First record, it should be: " + rec.getRecordName() 
								+ " and not " + schema.getRecord(rm.recordIdx).getRecordName());
					}
					
					arrayStart(null, jsonWriter, itemDtls.updateName(schema.getRecord(rm.recordIdx).getRecordName()));
					//writer.writeArrayFieldStart(itemDtls.updateName(schema.getRecord(rm.recordIdx).getRecordName()));
				    while (rm.line != null) {
						if (rm.recordIdx < 0) {
							logErrors.logErrorLine(rm.lineNumber, rm.line);
							rm.read();
						} else {							
							ItemRecordDtls recordDtls = recordItems.get(rm.recordIdx);
	
							switch (recordDtls.items.size()) {
							case 0: break;
							case 1:
								writeSingleTreeItemInArray(jsonWriter, rm, recordItems, rm.recordIdx, recordDtls.items.get(0));
								break;
							default:
								writeMultiTreeItemRecords(jsonWriter, rm, recordItems, rm.recordIdx, recordDtls);
							}
						}
				    }
				}		
			} else {
				int lineNo = 0;
				//writer.writeStartArray();
				
				arrayStart(null, jsonWriter, cobolSchemaDetails.copybookInformation.updateName(cobolSchemaDetails.schema.getLayoutName()));
				//writer.writeArrayFieldStart(cobolSchemaDetails.copybookInformation.updateName(cobolSchemaDetails.schema.getLayoutName()));
				while ((l = reader.read()) != null) {
					lineNo = writeLineAsRecord(jsonWriter, l, lineItemHelper, recordItems, lineNo);
				}
			}

			jsonWriter.writeEndArray();
			
			if (nameMainArray) {
				jsonWriter.writeEndObject();
			}
		} finally {
	        try {
				jsonWriter.close();
				//jsonStream.close();
	        } catch (IOException ioe) {
	        	throw ioe;
			} catch (Exception e) {
				e.printStackTrace();
			}
	        logErrors.reportErrors();
		}
	}
	
	private void arrayStart(IItem item, IJsonWriter jsonWriter, String fieldName) throws IOException {
		if (nameMainArray) {
			jsonWriter.writeStartObject();
			jsonWriter.writeArrayFieldStart(item, fieldName);
		} else {
			jsonWriter.writeStartArray(item);
		}
	}

	private int writeLineAsRecord(IJsonWriter jsonWriter, AbstractLine l, LineItemHelper lineItemHelper,
			List<ItemRecordDtls> recordItems, int lineNo) throws IOException {
		int recordIdx = l.getPreferredLayoutIdx();
		lineNo += 1;
		if (recordIdx < 0) {
			logErrors.logErrorLine(lineNo, l);
		} else {
			jsonWriter.writeStartObject();
			writeItems(jsonWriter, lineItemHelper.setLine(l), recordItems.get(recordIdx).items, new IntStack());
			jsonWriter.writeEndObject();
		}
		return lineNo;
	}
	
	private void doInit() throws IOException {
		cobolSchemaDetails = super.getCobolSchemaDetails();

		itemDtls = cobolSchemaDetails.copybookInformation;
		
        skipValidation = ! itemDtls.isRedefinedBinaryField();
	}
	
	
	private UpdateDetailsForSampleRecords doInitForSampleJson(boolean simulateExits) throws IOException {
		UpdateDetailsForSampleRecords updDetails = new UpdateDetailsForSampleRecords(super.getUpdateDetails(), simulateExits);
		cobolSchemaDetails = super.getCobolSchemaDetails(updDetails);

		itemDtls = cobolSchemaDetails.copybookInformation;
		
        skipValidation = ! itemDtls.isRedefinedBinaryField();
        
        return updDetails;
	}


	/**
	 * @param prettyPrint the prettyPrint to set
	 */
	@Override
	public final ICobol2Json setPrettyPrint(boolean prettyPrint) {
		this.prettyPrint = prettyPrint;
		return this;
	}

	@Override
	public ICobol2Json setNameMainArray(boolean nameMainArray) {
		this.nameMainArray = nameMainArray;
		return this;
	}

	private void writeItemInTree(IJsonWriter jsonWriter, ReadManager rm, List<ItemRecordDtls> recordItems) 
	throws IOException {


		if (rm.recordIdx < 0) {
			logErrors.logErrorLine(rm.lineNumber, rm.line);
		} else {
	  		int recIdx = rm.recordIdx;
	  		ItemRecordDtls recordDtls = recordItems.get(recIdx);
	  		
	  		switch (recordDtls.items.size()) {
	  		case 0: break;
	  		case 1:
				writeSingleTreeItemRecord(jsonWriter, rm, recordItems, recIdx, recordDtls.items.get(0));
				break;
			default:
				writeMultiTreeItemRecords(jsonWriter, rm, recordItems, recIdx, recordDtls);
	  		}
		}
	}
	
	/**
	 * @param writer
	 * @param rm
	 * @param recordItems
	 * @param recIdx
	 * @param recordDtls
	 * @throws IOException
	 */
	private void writeMultiTreeItemRecords(IJsonWriter jsonWriter, ReadManager rm, List<ItemRecordDtls> recordItems,
			int recIdx, ItemRecordDtls recordDtls) throws IOException {
		jsonWriter.writeStartObject();
		for (Item itm : recordDtls.items) {
			writeSingleTreeItemRecord(jsonWriter, rm, recordItems, recIdx, itm);
		}	
		jsonWriter.writeEndObject();
	}


	/**
	 * @param writer
	 * @param rm
	 * @param recordItems
	 * @param recIdx
	 * @param item
	 * @throws IOException
	 */
	private void writeSingleTreeItemRecord(
			IJsonWriter jsonWriter, ReadManager rm, List<ItemRecordDtls> recordItems,
			int recIdx, IItem item) throws IOException {
		
		jsonWriter.writeObjectFieldStart(item, item.getNameToUse());
		writeTreeItemRecord(jsonWriter, rm, recordItems, recIdx, item);
		jsonWriter.writeEndObject();
	}
	private void writeSingleTreeItemInArray(IJsonWriter jsonWriter, ReadManager rm, List<ItemRecordDtls> recordItems,
			int recIdx, IItem item) throws IOException {
		
//		jsonWriter.writeStartObject(item, item.getNameToUse());
		jsonWriter.writeStartObject();
		writeTreeItemRecord(jsonWriter, rm, recordItems, recIdx, item);
		jsonWriter.writeEndObject();
	}


	/**
	 * @param jsonWriter
	 * @param rm
	 * @param items
	 * @param recIdx
	 * @param item
	 * @throws IOException
	 */
	public void writeTreeItemRecord(IJsonWriter jsonWriter, ReadManager rm,
			List<ItemRecordDtls> recordItems, int recIdx, IItem item) throws IOException {
		
		if ( item.isOkToWriteItem(rm.line)) {
			if (item.getItemType() == IItem.TYPE_GROUP) {
				writeAnItem(jsonWriter, null, rm.lineItemHelper, item, new IntStack());
				writeChildren(jsonWriter, rm, recordItems, recIdx);	
			} else {
				jsonWriter.writeStartObject();
				writeAnItem(jsonWriter, null, rm.lineItemHelper, item, new IntStack());
				writeChildren(jsonWriter, rm, recordItems, recIdx);
				jsonWriter.writeEndObject();
			}
		}
	}

	/**
	 * @param jsonWriter
	 * @param rm
	 * @param items
	 * @param recIdx
	 * @throws IOException
	 */
	public void writeChildren(IJsonWriter jsonWriter, ReadManager rm,
			List<ItemRecordDtls> recordItems, int recIdx) throws IOException {
		rm.read();
		int lastIdx = -121;
		boolean first = true;
		while (rm.line != null
		&& rm.recordIdx >= 0
		&& rm.schema.getRecord(rm.recordIdx).getParentRecordIndex() == recIdx) {
			ItemRecordDtls recordDtls = recordItems.get(rm.recordIdx);
			RecordDetail record = recordDtls.record;
			
			switch (recordDtls.items.size()) {
			case 0: break;
			case 1:
				Item item = recordDtls.items.get(0);
				if (first) {
					jsonWriter.writeArrayFieldStart(item, item.getNameToUse());
				}  else if (lastIdx != rm.recordIdx) {
					jsonWriter.writeEndArray();
					jsonWriter.writeArrayFieldStart(item, item.getNameToUse());
				}
				lastIdx = rm.recordIdx;

				writeSingleTreeItemInArray(jsonWriter, rm, recordItems, rm.recordIdx, item);
				break;
			default:
				if (first) {
					jsonWriter.writeArrayFieldStart(recordDtls.items.get(0), record.getRecordName());
				}  else if (lastIdx != rm.recordIdx) {
					jsonWriter.writeEndArray();
					jsonWriter.writeArrayFieldStart(recordDtls.items.get(0), record.getRecordName());
				}
				lastIdx = rm.recordIdx;

				writeMultiTreeItemRecords(jsonWriter, rm, recordItems, recIdx, recordDtls);
			}
			
			first = false;
		}
		
		if (lastIdx >= 0) {
			jsonWriter.writeEndArray();
		}
	}
	
	private void writeItem(IJsonWriter jsonWriter, LineItemHelper l, IItem item, IntStack indexs) throws IOException {

		String name = item.getName();
		if (! item.isOkToWriteItem(l.getLine())) {
			
		} else if (name == null || item.getName().length() == 0 || "filler".equalsIgnoreCase(name)) {
			if (item.getItemType() == IItem.TYPE_GROUP) {
				if (item.getOccurs() != null && item.getOccurs() >= 1) {
					writeArray(jsonWriter, l, item, "filler", indexs);
				} else {
					writeItems(jsonWriter, l, item.getChildItems(), indexs);
				}
			}
		} else if (item.getOccurs() != null && item.getOccurs() > 1) {
			writeArray(jsonWriter, l, item, item.getNameToUse(), indexs);
		} else {
			writeAnItem(jsonWriter, item.getNameToUse(), l, item, indexs);
		}
	}

	/**
	 * @param jsonWriter
	 * @param l
	 * @param item
	 * @param indexs
	 * @throws IOException 
	 */
	private void writeAnItem(IJsonWriter jsonWriter, String fieldname, LineItemHelper l, IItem item,
			IntStack indexs) throws IOException {
		if (item.getItemType() == IItem.TYPE_GROUP) {
			if (fieldname == null || (isFlatten() && item.isCanBeFlattened())) {
				writeItems(jsonWriter, l, item.getChildItems(), indexs);
			} else {
				jsonWriter.writeObjectFieldStart(item, fieldname);
				writeItems(jsonWriter, l, item.getChildItems(), indexs);
				jsonWriter.writeEndObject();
			}
		} else if (indexs.size == 0) {
			writeField(jsonWriter, l.getLine(), fieldname, l.getFieldValue(item, null), item);
		} else {
			writeField(jsonWriter, l.getLine(), fieldname, l.getFieldValue(item, indexs.toArray()), item);
		}
	}

	/**
	 * @param jsonWriter
	 * @param l
	 * @param item
	 * @param indexs
	 * @throws IOException 
	 */
	private void writeArray(IJsonWriter jsonWriter, LineItemHelper l, IItem item, String name,
			IntStack indexs) throws IOException {
		int num = l.getArrayCount(item, indexs.toArray());
//		String dependingOn = item.getDependingOn();
//		IOccursDependingDetails fieldLookup = item.getDependingOnDetails();
//
//		int[] indexArray = indexs.toArray();
//		if (fieldLookup.isDependingOnArray()) {
//			try {
//				num = fieldLookup.getValue(l);
//			} catch (Exception e) { }
//		} else if (item.getArrayValidation() != null) {
//			num = item.getArrayValidation().getCount(l, item, indexArray, num);
//		}
		
		indexs.add(0);
		int[] indexArray = indexs.toArray();
		int id;
		
		boolean writeArrayStart = true;
		for (int i = 0; i < num; i++) {
			if (item.getArrayValidation() != null
			&& (id = l.checkArrayIndex(item, indexArray, i)) != IArrayItemCheck.R_PROCESS ) {
				if (id == IArrayItemCheck.R_STOP) {
					break;
				}
			} else { 
				if (writeArrayStart) {
					jsonWriter.writeArrayFieldStart(item, name);
					writeArrayStart = false;
				}
				if (item.getItemType() == IItem.TYPE_GROUP) {
					if ( item.isOkToWriteItem(l.getLine())) {
						jsonWriter.writeStartObject();
						writeItems(jsonWriter, l, item.getChildItems(), indexs.set(i));
						jsonWriter.writeEndObject();
					}
				} else {
					indexArray[indexArray.length - 1] = i;
					//writer.writeCharacters(l.getFieldValue(item.arrayDef.getField(indexArray)).asString());
					AbstractFieldValue val = l.getFieldValue(item, indexArray);
					
					String s = null;
					if (val.isNumeric()) {
						BigDecimal bd = getNumVal(val);

						if (bd != null) {
							if (item.isFormatFieldAvailable()) {
								jsonWriter.writeString(item, item.formatField(bd.toString()));
							} else {
								jsonWriter.writeNumber(item, bd);
							}
						} else {
							s = "";
						}
					} else {
						if ((s = getStrValue(val, item)) == null) {
							s = "";
						}
					}
					
					if (s != null) {
						//writer.writeString(val.asString()); I think it is wrong
						jsonWriter.writeString(item, s);
					}
				}
//				writer.writeEndArray();
			}
		}
		if (! writeArrayStart) {
			jsonWriter.writeEndArray();
		}
		indexs.remove();
	}
	
	private void writeItems(IJsonWriter jsonWriter, LineItemHelper l, List<? extends IItem> items, IntStack indexs) throws IOException {

		int i = 0;
		while (i < items.size()) {
			IItem item = items.get(i);
			if (item.getRedefineItemCount() > 0) { 		
				List<IItem> itemsToUses = item.getRedefinedItemsToUses(l.getLine());
				
				if (itemsToUses != null) {
					for (IItem redefItm : itemsToUses) {
						writeItem(jsonWriter, l, redefItm, indexs);
					}
				}
				
				i += item.getRedefineItemCount();
			} else {
				writeItem(jsonWriter, l, item, indexs);
				i += 1;
			}
		}
//		for (IItem item : items) {
//			writeItem(writer, l, item, indexs);
//		}
	}
	
	private void writeField(IJsonWriter jsonWriter, AbstractLine line, String fieldName, AbstractFieldValue val, IItem item) throws IOException {
		try {
			if (val.isNumeric()) {
				try {
					BigDecimal bd = getNumVal(val);
					if (bd != null) {
						if (item.isFormatFieldAvailable()) {
							String formatField = item.formatField(bd.toString());
							//System.out.println(formatField);
							jsonWriter.writeStringField(item, fieldName, formatField);
						} else {
							jsonWriter.writeNumberField(item, fieldName, bd);
						}
					}
				} catch (Exception e) {	}
			} else {
				String formatedField = getStrValue(val, item);
				if (formatedField != null && formatedField.trim().length() > 0) {
					jsonWriter.writeStringField(item, fieldName, formatedField);
				}
			}
		} catch (Exception e) {
		}
	}
	
	private BigDecimal getNumVal(AbstractFieldValue val) {
		BigDecimal ret = null;
		
		try {
			ret = val.asBigDecimal();
		} catch (Exception e) {	}
		
		return ret;
	}
	
	private String getStrValue(AbstractFieldValue val, IItem item) {
		String ret = null;
		
		try {
			String s = item.formatField(val.asString());
			if ( s != null && (skipValidation || (! item.isFieldRedefined()) || isValidString(s))) {
				ret = s;
			}
		} catch (Exception e) {	}
		
		return ret;
	}
	
	
	@Override
	public ICobol2Json jsonArrayToCobolFile(String jsonFileName, String outFileName) throws IOException {
		ISchemaIOBuilder ioBuilder = asIOBuilder();
		
		(new JsonToCobol())
			.readJson(jsonFileName,
					new ToJRecordFile(ioBuilder, ioBuilder.newWriter(outFileName)), 
					super.getRenameToCobol());
		return this;
	}
	

	@Override
	public List<AbstractLine> jsonArrayToCobolLines(Reader jsonReader) throws IOException {
		ListLineWriter lw = new ListLineWriter();
		(new JsonToCobol()).readJson(
				jsonReader,
				new ToJRecordFile(asIOBuilder(), lw), 
				super.getRenameToCobol());
		return lw.getLines();
	}
	

	@Override
	public ICobol2Json jsonArrayToCobolFile(Reader jsonReader, OutputStream outStream) throws IOException {
		ISchemaIOBuilder ioBuilder = asIOBuilder();
		
		(new JsonToCobol()).readJson(
				jsonReader,
				new ToJRecordFile(ioBuilder, ioBuilder.newWriter(outStream)), 
				super.getRenameToCobol());
		return this;
	}

	@Override
	public ICobol2Json jsonObjectToCobolFile(String jsonFileName, String outFileName) throws IOException {
		ISchemaIOBuilder ioBuilder = asIOBuilder();
		
		(new JsonToCobol()).readJson(
				jsonFileName,
				new ToJRecordFile(ioBuilder, ioBuilder.newWriter(outFileName), true), 
				super.getRenameToCobol());
		return this;
	}
	

	@Override
	public ICobol2Json jsonObjectToCobolFile(Reader jsonReader, OutputStream outStream) throws IOException {
		ISchemaIOBuilder ioBuilder = asIOBuilder();
		
		(new JsonToCobol()).readJson(
				jsonReader, 
				new ToJRecordFile(ioBuilder, ioBuilder.newWriter(outStream), true), 
				super.getRenameToCobol());
		return this;
	}
	

	@Override
	public byte[] jsonStringToSingleCobolRecord(String json) throws IOException {
		AbstractLine line = asIOBuilder().newLine();
		
		(new JsonToCobol()).processJson(json, new ToJRecordLine(line), super.getRenameToCobol());
		return line.getData();
	}

	/**
	 * @param fl
	 */
	private static boolean isValidString(String fl) {
		for (int j = 0; j < fl.length(); j++) {
			char ch = fl.charAt(j);
			switch (Character.getType(ch)) {
			case  Character.COMBINING_SPACING_MARK:
			case  Character.CONNECTOR_PUNCTUATION:
			case  Character.CURRENCY_SYMBOL:
			case  Character.DASH_PUNCTUATION:
			case  Character.DECIMAL_DIGIT_NUMBER:
			case  Character.ENCLOSING_MARK:
			case  Character.END_PUNCTUATION:
			case  Character.FINAL_QUOTE_PUNCTUATION:
			case  Character.FORMAT:
			case  Character.INITIAL_QUOTE_PUNCTUATION:
			case  Character.LETTER_NUMBER:
			case  Character.LINE_SEPARATOR:
			case  Character.LOWERCASE_LETTER:
			case  Character.MATH_SYMBOL:
			case  Character.MODIFIER_LETTER:
			case  Character.MODIFIER_SYMBOL:
			case  Character.NON_SPACING_MARK:
			case  Character.OTHER_LETTER:
			case  Character.OTHER_NUMBER:
			case  Character.OTHER_PUNCTUATION:
			case  Character.OTHER_SYMBOL:
			case  Character.PARAGRAPH_SEPARATOR:
			case  Character.SPACE_SEPARATOR:
			case  Character.START_PUNCTUATION:
			case  Character.SURROGATE:
			case  Character.TITLECASE_LETTER:
			case  Character.UPPERCASE_LETTER:
				break;
			// Should be: Character.CONTROL, Character.UNASSIGNED, Character.PRIVATE_USE
			default:
				//System.out.print('*' + " " + ch + "~" + Character.getType(ch));
				return false;
			}
		}
		return true;
	}

//	@Override
//	public void json2Cobol(String xmlFileName, String cobolFileName) 
//	throws RecordException, IOException,  XMLStreamException {
//		json2Cobol(new FileInputStream(xmlFileName), new BufferedOutputStream(new FileOutputStream(cobolFileName), 0x4000));
//	}
//
//	@Override
//	public void json2Cobol(InputStream xmlStream, OutputStream cobolStream) 
//	throws RecordException, IOException,  XMLStreamException{
//		//XMLInputFactory f = XMLInputFactory.newInstance();
//		doInit();
//		if (itemDtls.getDuplicateFieldsStatus() == UpdateSchemaItems.D_DUPLICATES) {
//			throw new RuntimeException("Duplicate names are not supported for Xml --> Cobol");
//		}
////		String spaces = "                                                                                                  ";
//		String lastName = "", name;
//		int lvl = 0;
//		JsonToken lastType, type = JsonToken.VALUE_NULL;
//		JsonFactory jfactory = new JsonFactory();
//
//		/*** read from file ***/
//		JsonParser parser = jfactory.createParser(xmlStream);
//		//XMLStreamReader parser = XMLInputFactory.newInstance().createXMLStreamReader(xmlStream);
//		AbstractLine l = null;
//		AbstractLineWriter w = iob.newWriter(cobolStream);		
//		StringBuilder b = new StringBuilder();
//		Map<String, ? extends IItem> arrayItems = itemDtls.getArrayItems();
//		IntStack arrayDtls = new IntStack();
//		IntStack levelNames = new IntStack();
//		IGetRecordFieldByName fieldLookup = itemDtls.getFieldLookup();
//		Map<String, Integer> recordHierarchyMap = itemDtls.getRecordHierarchyMap();
//		int maxHierarchLvl = itemDtls.getMaxRecordHierarchyLevel();
//		String recordName = "";
//		Integer lookupLvl;
//		boolean lastWasArray = false;
//		
//
//		lastType = type;
//		type = parser.nextToken();
//		
//		switch (type) {
//		case START_ARRAY:
//			break;
//		case FIELD_NAME:
//			String n = parser.getText();
//			type = parser.nextToken();
//			
////			switch (type) {
////			case START_ARRAY:
////				break;
////			case START_OBJECT:
////				//TODO single Object
////				break;
////			default:
////				throw new RecordException("Invalid JSon, Expecting Array/Object and  not a " + type.name());
////			}
//			break;
//		default:
//			throw new RecordException("Invalid JSon, was not expecting a " + type.name() + " at the start");
//		}
////		while ((type = parser.nextToken()) != null) {
////			
////			switch (type) {
////			case START_OBJECT:
////            	lvl += 1;
////            	name = parser.getName().toString();
////            	if (lvl == 2 
////                || (     maxHierarchLvl >= lvl - 3
////                   &&	(lookupLvl = recordHierarchyMap.get(name.toUpperCase())) != null
////             	   &&    lookupLvl >= lvl - 3 
////             		)) {
////            		if (l != null) {
//////           			System.out.println();
//////            			System.out.println(l.getFullLine());
////            			w.write(l);
////            		}
////        			recordName = name;
////
////            		l = iob.newLine();
////            		if (schema.getRecordCount() > 1) {
////            			int recIdx = schema.getRecordIndex(name);
////            			if (recIdx >= 0) {
////            				l.setWriteLayout(recIdx);
////            			}
////            		}
//////            	} else if ((lookupLvl = recordHierarchyMap.get(name.toUpperCase())) != null) {
//////            		System.out.println("## " + name + " " + lookupLvl + " " + lvl );
//////            	} else if (lvl <= 4 && name.indexOf("Record") >= 0) {
//////            		System.out.println("** " + name + " " + lvl );
////            	}
//////            	System.out.println();
//////           	System.out.print(spaces.substring(spaces.length() - 2 * lvl +1) + parser.getName() + " >");
////            	String ucName;
////            	if (name != null && arrayItems.containsKey((ucName = name.toUpperCase()))) {
////            		if (name.equalsIgnoreCase(levelNames.getLastName())) {
////            			arrayDtls.inc();
////            		} else {
////            			arrayDtls.add(0, name, arrayItems.get(ucName));
////            		}
////            	}
////            	
////            	lastName = name;
////            	levelNames.add(0, name, null);
////            	b.setLength(0);
////            	
////            	
////            	break;
////            case END_OBJECT:
////            	String name2 = parser.getName().toString();
////    			int[] indexes = arrayDtls.toArray();
////            	
//////				System.out.print(b + "< " + name2 );
////				
////				if (lastName.equals(name2)) {
////	        		IFieldDetail f;
////	        		String n = lastName;
////	        		
////	        		f = fieldLookup.getField(recordName, n, indexes);
////	        		
////	        		if (f == null) {
////	        			if (b.length() > 0) {
////	        				f = fieldLookup.getField(recordName, n, indexes);
////	        				throw new RuntimeException("Field: " + n + " does not exist, can not assign '" + b.toString() + "'");
////	        			}
////	        		} else {
////		        		AbstractFieldValue fieldValue = l.getFieldValue(f);
////			        	if (lastType == XMLStreamConstants.START_ELEMENT) {
////							fieldValue.set(CommonBits.NULL_VALUE);
////			        	} else {
////			        		String txt = b.toString();
////			        		if (fieldValue.isNumeric()) {
////			        			txt = txt.trim();
////			        		}
////							fieldValue.set(txt);
////			        	}
////	        		}
////		        	b.setLength(0); 	
////				}
////				
////				if (lastWasArray) {
////					IItem item = arrayDtls.getLastItem();
////					if (item != null && item.arrayCheck != null && arrayDtls.size >= 0) {
////						item.arrayCheck.updateForCount(l, item, indexes, arrayDtls.stack[arrayDtls.size]+1);
////					}					
////				}
////				lastWasArray = false;
////				if (name2 != null && name2.equalsIgnoreCase(arrayDtls.getName())) {
////					arrayDtls.remove();
////					lastWasArray = true;
////				}
////				levelNames.remove();
////            	lvl -= 1;
////            	break;
////            case CHARACTERS:
////            	String text = parser.getText();
////            	
////            	b.append(text);
////            	
//// //           	System.out.print(text.trim());
////            	break;
//////            case (XMLStreamConstants.START_DOCUMENT) :
//////            break;
//////            case (XMLStreamConstants.COMMENT) :
//////            break;
//////            case (XMLStreamConstants.DTD) :
//////            	break;
//////            case (XMLStreamConstants.ENTITY_REFERENCE) :
//////            	break;
//////            case (XMLStreamConstants.CDATA) :
//////              break;
//////            case (XMLStreamConstants.END_DOCUMENT): 
//////            	break;
////          	default:
////			}
////			lastType = type;
////		}
//		
////		if (l != null) {
////			w.write(l);
////		}
//		parser.close();
//		xmlStream.close();
//		w.close();
//		cobolStream.close();
//	}
	
//	private void processJson(String recordType, JsonParser parser, JsonToken type) throws JsonParseException, IOException {
//		//TODO write
//		//TODO write
//		//TODO write
//		//TODO write
//		Map<String, ? extends IItem> arrayItems = itemDtls.getArrayItems();
//		IntStack arrayDtls = new IntStack();
//		IntStack levelNames = new IntStack();
//		IGetRecordFieldByName fieldLookup = itemDtls.getFieldLookup();
//		Map<String, Integer> recordHierarchyMap = itemDtls.getRecordHierarchyMap();
//		int maxHierarchLvl = itemDtls.getMaxRecordHierarchyLevel();
//		JsonToken lastType = type;
//		IntStack stack = new IntStack();
//		IntStack nameStack = new IntStack();
//		AbstractLine l = null; //iob.newLine();
//		String currentName, name = recordType;
//
//		
//		while ((type = parser.nextToken()) != null) {
//			currentName = "";
//			switch (type) {
//			case START_ARRAY:			
//				IItem item = null;
//				if (nameStack.size > 0) {
//					//fieldLookup.getField(recordType, name, arrayDtls.toArray());
//					item = arrayItems.get(name.toUpperCase());
//					arrayDtls.add(0);
//				}
//				nameStack.add(-1, name, item, type);
//				break;
//			case END_ARRAY:
//				nameStack.remove();
//				break;
//			case START_OBJECT:
//				if (parser.getCurrentName() == null) {
//					
//				} else {
//					
//				}
//				break;
//			case END_OBJECT:
//				break;
//			case FIELD_NAME:
//				currentName = parser.getText();
//				nameStack.set(currentName); 
//				break;
//			}
//			
//			name = currentName;
//		}
//	}
	
	/**
	 * Class to keep track of Cobol Group Levels
	 * (in particular Groups that are arrays) 
	 * @author Bruce Martin
	 *
	 */
	private static class IntStack {
		private int[] stack = new int[100];
		private String[] names = new String[100];
//		private IItem[] items = new IItem[100];
//		private JsonToken[] tokens = new JsonToken[100];
		private int size = 0;
		
		public IntStack add(int item) {
			stack[size++] = item;
			return this;
		}
		
//		public IntStack add(int pos, String name, IItem item, JsonToken token) {
//			stack[size] = pos;
//			items[size] = item;
//			tokens[size] = token;
//			names[size++] = name;
//			
//			return this;
//		}
		
		public IntStack set(int item) {
			stack[size - 1] = item;
			return this;
		}
//		public IntStack set(String name) {
//			stack[size - 1] = 0;
//			names[size - 1] = name;
//			return this;
//		}
//	
//		public IntStack inc() {
//			size += 1;
//			stack[size - 1] += 1;
//			return this;
//		}
//
//		public String getName() {
//			if (size <= 0) return "";
//			
//			return names[size-1];
//		}
//
//		public String getLastName() {
//			if (size < 0) return "";
//			
//			return names[size];
//		}
//
//		public IItem getLastItem() {
//			if (size < 0) return null;
//			return items[size];
//		}
//		
		public void remove() {
			names[size+1] = null;
			size -= 1;
		}
		
		public int[] toArray() {
			int[] ret = new int[size];
			System.arraycopy(stack, 0, ret, 0, size);
			return ret;
		}	
	}

	
	public static ICobol2Json newCobol2Json(String cobolCopybook) {
		return new Cobol2JsonImp(cobolCopybook, new CobolCopybookLoader());
	}
	
	
	public static ICobol2Json newCobol2Json(InputStream cobolCopybook, String copybookName) {
		return new Cobol2JsonImp(cobolCopybook, copybookName, new CobolCopybookLoader());
	}
	
	public static ICobol2Json newCobol2Json(Reader cobolCopybookReader, String copybookName) {
		return new Cobol2JsonImp(cobolCopybookReader, copybookName, new CobolCopybookLoader());
	}

	
	public static Icb2xml2Json newCb2Xml2Json(String cobolCopybook) {
		return new Cobol2JsonImp(cobolCopybook, new XmlCopybookLoader());
	}
	
	
	public static Icb2xml2Json newCb2Xml2Json(InputStream cobolCopybook, String copybookName) {
		return new Cobol2JsonImp(cobolCopybook, copybookName, new XmlCopybookLoader());
	}

	private static class ReadManager {
		final IReadLine reader;
		final LayoutDetail schema;
		AbstractLine line;
		int recordIdx, lineNumber = 0;
		final LineItemHelper lineItemHelper;
		
		ReadManager(IReadLine r, LayoutDetail schema, LineItemHelper lineItemHelper) {
			super();
			this.reader = r;
			this.schema = schema;
			this.lineItemHelper = lineItemHelper;
		}
		
		void read() throws IOException {
			line = reader.read();
			if (line != null) {
				lineNumber += 1;
				recordIdx = line.getPreferredLayoutIdx();
				lineItemHelper.setLine(line);
			}
		} 
	}
}
