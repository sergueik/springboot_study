package net.sf.cobolToJson.impl.jsonWriter;

import java.io.IOException;
import java.io.OutputStream;
import java.io.Writer;
import java.math.BigDecimal;

import com.fasterxml.jackson.core.JsonFactory;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.util.DefaultPrettyPrinter;

import net.sf.JRecord.schema.jaxb.IItem;


public class JsonWriter implements IJsonWriter {
	private final JsonGenerator jsonGen;
	
	public JsonWriter(Writer writer) throws IOException {
		jsonGen = new JsonFactory().createGenerator(writer);
	}
	
	
	public JsonWriter(OutputStream jsonStream) throws IOException {
		jsonGen = new JsonFactory().createGenerator(jsonStream);
	}

	
	@Override
	public void initWriter(boolean pretty) {
		jsonGen.enable(JsonGenerator.Feature.AUTO_CLOSE_JSON_CONTENT);
		jsonGen.enable(JsonGenerator.Feature.FLUSH_PASSED_TO_STREAM);
        if (pretty) {
        	jsonGen.setPrettyPrinter(new DefaultPrettyPrinter());
        }

	}


	@Override
	public void writeStartArray(IItem item) throws IOException {
		jsonGen.writeStartArray();
	}

	@Override
	public void writeArrayFieldStart(IItem item, String fieldName) throws IOException {
		jsonGen.writeArrayFieldStart(fieldName);
	}

	@Override
	public void writeEndArray() throws IOException {
		jsonGen.writeEndArray();
	}

	@Override
	public void writeStartObject() throws IOException {
		jsonGen.writeStartObject();
	}

	@Override
	public void writeStartObject(IItem item, Object forValue) throws IOException {
		jsonGen.writeStartObject(forValue);
	}


	@Override
	public void writeObjectFieldStart(IItem item,String fieldName) throws IOException {
		jsonGen. writeObjectFieldStart(fieldName);
	}

	@Override
	public void writeEndObject() throws IOException {
		jsonGen.writeEndObject();
	}

	@Override
	public void writeString(IItem item, String text) throws IOException {
		jsonGen.writeString(text);
	}

	@Override
	public void writeNumber(IItem item, BigDecimal v) throws IOException {
		jsonGen.writeNumber(v);
	}

	@Override
	public void writeStringField(IItem item, String fieldName, String value) throws IOException {
		jsonGen.writeStringField(fieldName, value);
	}

	@Override
	public void writeNumberField(IItem item, String fieldName, BigDecimal value) throws IOException {
		jsonGen.writeNumberField(fieldName, value);
	}
	
	@Override
	public void close() throws IOException {
		jsonGen.flush();
		jsonGen.close();
		
	}
}
