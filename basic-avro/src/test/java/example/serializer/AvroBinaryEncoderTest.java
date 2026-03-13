package example.serializer;

import org.apache.avro.Schema;
import org.apache.avro.file.DataFileReader;
import org.apache.avro.generic.GenericDatumReader;
import org.apache.avro.generic.GenericRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class AvroBinaryEncoderTest {

    private AvroBinaryEncoder encoder;

    @TempDir
    Path tempDir;

    @BeforeEach
    void setUp() {
        encoder = new AvroBinaryEncoder();
    }

    @Test
    void shouldEncodeSimpleJsonToAvro() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "SimpleRecord",
                  "fields": [
                    {"name": "name", "type": "string"},
                    {"name": "age", "type": "int"}
                  ]
                }
                """);

        String json = """
                {"name": "John", "age": 30}
                """;

        File outputFile = tempDir.resolve("output.avro").toFile();
        encoder.encode(schema, json, outputFile.getAbsolutePath());

        assertThat(outputFile).exists();
        assertThat(outputFile.length()).isGreaterThan(0);
    }

    @Test
    void shouldRoundTripEncodeAndDecode() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Person",
                  "fields": [
                    {"name": "name", "type": "string"},
                    {"name": "age", "type": "int"},
                    {"name": "active", "type": "boolean"}
                  ]
                }
                """);

        String json = """
                {"name": "Alice", "age": 25, "active": true}
                """;

        File outputFile = tempDir.resolve("person.avro").toFile();
        encoder.encode(schema, json, outputFile.getAbsolutePath());

        // Read back and verify
        GenericDatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
        try (DataFileReader<GenericRecord> dataFileReader = new DataFileReader<>(outputFile, reader)) {
            assertThat(dataFileReader.hasNext()).isTrue();
            GenericRecord record = dataFileReader.next();

            assertThat(record.get("name").toString()).isEqualTo("Alice");
            assertThat(record.get("age")).isEqualTo(25);
            assertThat(record.get("active")).isEqualTo(true);
        }
    }

    @Test
    void shouldEncodeRecordWithNullableFields() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "NullableRecord",
                  "fields": [
                    {"name": "required", "type": "string"},
                    {"name": "optional", "type": ["null", "string"], "default": null}
                  ]
                }
                """);

        String json = """
                {"required": "hello", "optional": null}
                """;

        File outputFile = tempDir.resolve("nullable.avro").toFile();
        encoder.encode(schema, json, outputFile.getAbsolutePath());

        GenericDatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
        try (DataFileReader<GenericRecord> dataFileReader = new DataFileReader<>(outputFile, reader)) {
            GenericRecord record = dataFileReader.next();
            assertThat(record.get("required").toString()).isEqualTo("hello");
            assertThat(record.get("optional")).isNull();
        }
    }

    @Test
    void shouldEncodeFromFile() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "FileRecord",
                  "fields": [
                    {"name": "message", "type": "string"}
                  ]
                }
                """);

        // Write JSON to a temp file
        File jsonFile = tempDir.resolve("input.json").toFile();
        java.nio.file.Files.writeString(jsonFile.toPath(), """
                {"message": "from file"}
                """);

        File outputFile = tempDir.resolve("from-file.avro").toFile();
        encoder.encodeFromFile(schema, jsonFile.getAbsolutePath(), outputFile.getAbsolutePath());

        GenericDatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
        try (DataFileReader<GenericRecord> dataFileReader = new DataFileReader<>(outputFile, reader)) {
            GenericRecord record = dataFileReader.next();
            assertThat(record.get("message").toString()).isEqualTo("from file");
        }
    }

    @Test
    void shouldEncodeRecordWithEnum() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "EnumRecord",
                  "fields": [
                    {"name": "status", "type": {"type": "enum", "name": "Status", "symbols": ["ACTIVE", "INACTIVE"]}}
                  ]
                }
                """);

        String json = """
                {"status": "ACTIVE"}
                """;

        File outputFile = tempDir.resolve("enum.avro").toFile();
        encoder.encode(schema, json, outputFile.getAbsolutePath());

        GenericDatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
        try (DataFileReader<GenericRecord> dataFileReader = new DataFileReader<>(outputFile, reader)) {
            GenericRecord record = dataFileReader.next();
            assertThat(record.get("status").toString()).isEqualTo("ACTIVE");
        }
    }

    @Test
    void shouldFailWithInvalidJson() {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "name", "type": "string"}
                  ]
                }
                """);

        File outputFile = tempDir.resolve("fail.avro").toFile();

        assertThatThrownBy(() -> encoder.encode(schema, "not valid json", outputFile.getAbsolutePath()))
                .isInstanceOf(IOException.class);
    }
}
