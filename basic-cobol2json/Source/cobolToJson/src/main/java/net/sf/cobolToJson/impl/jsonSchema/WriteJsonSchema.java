package net.sf.cobolToJson.impl.jsonSchema;

import java.io.IOException;
import java.io.Writer;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;

public class WriteJsonSchema {
	//private final Writer writer;
	private final JsonGenerator jsonGen;
	
	public WriteJsonSchema(Writer writer, JsonItem item) throws IOException {
		jsonGen = new JsonFactory().createGenerator(writer);
		
		jsonGen.useDefaultPrettyPrinter();
		
		
		try {
			writeItem(item, true);
		} finally {
			jsonGen.close();
		}
	}
	
	private void writeItem(JsonItem item, boolean first) throws IOException {
		JsonType jsonType = item.getJsonType();
		switch (jsonType) {
		case ARRAY:
		case OBJECT:
			writeSingleJsonItem(item, jsonType, first);
			break;
		default:
			System.out.println(">> >" + item.getName() + "<<");
			String name = item.getName();
			if (name == null || name.length() == 0) {
				jsonGen.writeStringField("type", jsonType.typeName);
			} else {
				jsonGen.writeObjectFieldStart(name);
				jsonGen.writeStringField("type", jsonType.typeName);
				jsonGen.writeEndObject();
			}
		}
	}

	private void writeSingleJsonItem(JsonItem item, JsonType jsonType, boolean first) throws IOException {
		String name = item.getName();
		System.out.println(name);
		if (first) {
			jsonGen.writeStartObject();;
			jsonGen.writeStringField("$schema", "http://json-schema.org/draft-06/schema#");
			writeOjectDetails(item, jsonType);
			jsonGen.writeEndObject();
		} else if (name != null && name.length() > 0) {
			jsonGen.writeObjectFieldStart(name);
			writeOjectDetails(item, jsonType);
			jsonGen.writeEndObject();
		} else if (item.parent != null && item.parent.getJsonType() == JsonType.ARRAY) {
//			jsonGen.writeStartObject();
			writeOjectDetails(item, jsonType);
//			jsonGen.writeEndObject();
		} else {
			for (JsonItem child : item.getChildren()) {
				writeItem(child, false);
			}
		}
	}

	private void writeOjectDetails(JsonItem item, JsonType jsonType) throws IOException {
		jsonGen.writeStringField("type", jsonType.typeName);
		writeObjectStart(jsonType.nameOfChildren);
		for (JsonItem child : item.getChildren()) {
			writeItem(child, false);
		}
		jsonGen.writeEndObject();
	}
	
	private void writeObjectStart(String name) throws IOException {
		//System.out.println(name);
		if (name == null || name.length() == 0) {
			jsonGen.writeStartObject();
		} else {
			//jsonGen.writeStartObject(name);
			jsonGen.writeObjectFieldStart(name);
		}

	}
	
}
