package net.sf.cobolToJson.impl.jsonWriter;

import java.io.IOException;
import java.math.BigDecimal;

import net.sf.JRecord.schema.jaxb.IItem;

public interface IJsonWriter {

	void initWriter(boolean pretty);

	void writeStartArray(IItem item) throws IOException;

	void writeArrayFieldStart(IItem item, String fieldName) throws IOException;

	void writeEndArray() throws IOException;

	void writeStartObject() throws IOException;

	void writeStartObject(IItem item, Object forValue) throws IOException;

	void writeObjectFieldStart(IItem item, String fieldName) throws IOException;

	void writeEndObject() throws IOException;

	void writeString(IItem item, String text) throws IOException;

	void writeNumber(IItem item, BigDecimal v) throws IOException;

	void writeStringField(IItem item, String fieldName, String value) throws IOException;

	void writeNumberField(IItem item, String fieldName, BigDecimal value) throws IOException;

	void close() throws IOException;

}