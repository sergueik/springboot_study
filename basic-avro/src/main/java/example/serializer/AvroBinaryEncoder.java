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

/**
 * Encodes JSON data into Avro binary format (.avro container file).
 * The output file contains the Avro header, embedded schema, and binary-encoded data.
 */
public class AvroBinaryEncoder {

    /**
     * Encode a JSON string into an Avro binary file.
     *
     * @param schema     the Avro schema
     * @param jsonInput  the JSON string to encode
     * @param outputPath path to the output .avro file
     * @throws IOException if encoding or writing fails
     */
    public void encode(Schema schema, String jsonInput, String outputPath) throws IOException {
        GenericRecord record = parseJsonToRecord(schema, jsonInput);
        writeToAvroFile(schema, record, outputPath);
    }

    /**
     * Encode a JSON file into an Avro binary file.
     *
     * @param schema       the Avro schema
     * @param jsonFilePath path to the input JSON file
     * @param outputPath   path to the output .avro file
     * @throws IOException if reading, encoding, or writing fails
     */
    public void encodeFromFile(Schema schema, String jsonFilePath, String outputPath) throws IOException {
        String jsonContent = Files.readString(Path.of(jsonFilePath));
        encode(schema, jsonContent, outputPath);
    }

    /**
     * Parse a JSON string into a GenericRecord using the given schema.
     */
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
