package example.serializer;

import org.apache.avro.Schema;
import org.apache.avro.file.DataFileWriter;
import org.apache.avro.generic.GenericDatumReader;
import org.apache.avro.generic.GenericDatumWriter;
import org.apache.avro.generic.GenericRecord;
import org.apache.avro.io.DatumReader;
import org.apache.avro.io.DecoderFactory;
import org.apache.avro.io.JsonDecoder;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class AvroBinaryEncoder {

	public void encode(Schema schema, String jsonInput, String outputPath) throws IOException {
		GenericRecord record = parseJsonToRecord(schema, jsonInput);
		writeToAvroFile(schema, record, outputPath);
	}

	public void encodeFromFile(Schema schema, String jsonFilePath, String outputPath) throws IOException {
		String jsonContent = Files.readString(Path.of(jsonFilePath));
		encode(schema, jsonContent, outputPath);
	}

	GenericRecord parseJsonToRecord(Schema schema, String json) throws IOException {
		DatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
		JsonDecoder decoder = DecoderFactory.get().jsonDecoder(schema, json);
		return reader.read(null, decoder);
	}

	private void writeToAvroFile(Schema schema, GenericRecord record, String outputPath) throws IOException {
		GenericDatumWriter<GenericRecord> writer = new GenericDatumWriter<>(schema);
		try (DataFileWriter<GenericRecord> dataFileWriter = new DataFileWriter<>(writer)) {
			dataFileWriter.create(schema, new File(outputPath));
			dataFileWriter.append(record);
		}
	}
}
