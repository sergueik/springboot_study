package example.serializer;

import org.apache.avro.Schema;
import org.apache.avro.file.DataFileReader;
import org.apache.avro.generic.GenericDatumReader;
import org.apache.avro.generic.GenericRecord;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import com.fasterxml.jackson.databind.JsonNode;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.equalTo;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.avro.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;

class AvroBinaryEncoderTest {

	private AvroBinaryEncoder encoder;

	@TempDir
	Path tempDir;

	@BeforeEach
	void setUp() {
		encoder = new AvroBinaryEncoder();
	}

	@Disabled("org.apache.avro.AvroTypeException: Expected field name not found: message")
	@Test
	void shouldEncodeSimpleJsonToAvro() throws IOException {
		Schema schema = loadSchema("schemas/simple.avsc");
		String json = "{\"name\": \"John\", \"age\": 30}";

		File outputFile = tempDir.resolve("output.avro").toFile();
		encoder.encode(schema, json, outputFile.getAbsolutePath());

		assertThat(outputFile.exists(), is(true));
		assertThat(outputFile.length(), greaterThan(0L));
	}

	@Test
	void shouldRoundTripEncodeAndDecode() throws IOException {
		Schema schema = loadSchema("schemas/roundtrip.avsc");
		String json = "{\"name\": \"Alice\", \"age\": 25, \"active\": true}";

		File outputFile = tempDir.resolve("person.avro").toFile();
		encoder.encode(schema, json, outputFile.getAbsolutePath());

		// Read back and verify
		GenericDatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
		try (DataFileReader<GenericRecord> dataFileReader = new DataFileReader<>(outputFile, reader)) {
			assertThat(dataFileReader.hasNext(), is(true));
			GenericRecord record = dataFileReader.next();

			assertThat(record.get("name").toString(), equalTo("Alice"));
			assertThat(record.get("age"), equalTo(25));
			assertThat(record.get("active"), is(true));
		}
	}

	@Test
	void shouldEncodeRecordWithNullableFields() throws IOException {
		Schema schema = loadSchema("schemas/nullable.avsc");

		String json = "{\"required\": \"hello\", \"optional\": null}";

		File outputFile = tempDir.resolve("nullable.avro").toFile();
		encoder.encode(schema, json, outputFile.getAbsolutePath());

		GenericDatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
		try (DataFileReader<GenericRecord> dataFileReader = new DataFileReader<>(outputFile, reader)) {
			GenericRecord record = dataFileReader.next();
			assertThat(record.get("required").toString(), equalTo("hello"));
			assertThat(record.get("optional"), nullValue());
		}
	}

	@Test
	void shouldEncodeFromFile() throws IOException {
		Schema schema = loadSchema("schemas/example.avsc");
		// Write JSON to a temp file
		File jsonFile = tempDir.resolve("input.json").toFile();
		java.nio.file.Files.writeString(jsonFile.toPath(), "{\"message\": \"from file\"}");

		File outputFile = tempDir.resolve("from-file.avro").toFile();
		encoder.encodeFromFile(schema, jsonFile.getAbsolutePath(), outputFile.getAbsolutePath());

		GenericDatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
		try (DataFileReader<GenericRecord> dataFileReader = new DataFileReader<>(outputFile, reader)) {
			GenericRecord record = dataFileReader.next();
			assertThat(record.get("message").toString(), equalTo("from file"));
		}
	}

	@Test
	void shouldEncodeRecordWithEnum() throws IOException {
		Schema schema = loadSchema("schemas/enum.avsc");
		String json = "{\"status\": \"ACTIVE\"}";
		File outputFile = tempDir.resolve("enum.avro").toFile();
		encoder.encode(schema, json, outputFile.getAbsolutePath());

		GenericDatumReader<GenericRecord> reader = new GenericDatumReader<>(schema);
		try (DataFileReader<GenericRecord> dataFileReader = new DataFileReader<>(outputFile, reader)) {
			GenericRecord record = dataFileReader.next();
			assertThat(record.get("status").toString(), equalTo("ACTIVE"));
		}
	}

	@Test
	void shouldFailWithInvalidJson() throws IOException {
		Schema schema = loadSchema("schemas/invalid.json");
		File outputFile = tempDir.resolve("fail.avro").toFile();
		IOException exception = assertThrows(IOException.class,
				() -> encoder.encode(schema, "not valid json", outputFile.getAbsolutePath()));
	}

	private Schema loadSchema(String resource) throws IOException {
		try (java.io.InputStream is = getClass().getClassLoader().getResourceAsStream(resource)) {
			return new Schema.Parser().parse(is);
		}
	}

}
