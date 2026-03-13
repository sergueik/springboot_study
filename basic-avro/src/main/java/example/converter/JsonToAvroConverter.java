package example.converter;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import example.converter.interfaces.TypeDetector;
import example.model.AvroTypeInfo;
import example.util.EnumDetector;
import example.util.UuidDetector;
import org.apache.avro.Schema;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

public class JsonToAvroConverter {

	private final ObjectMapper objectMapper;
	private final TypeInferenceEngine inferenceEngine;
	private final SchemaGenerator schemaGenerator;

	public JsonToAvroConverter() {
		this.objectMapper = new ObjectMapper();

		List<TypeDetector> detectors = Arrays.asList(new UuidDetector(), new EnumDetector());

		this.inferenceEngine = new TypeInferenceEngine(detectors);
		this.schemaGenerator = new SchemaGenerator();
	}

	public JsonToAvroConverter(ObjectMapper objectMapper, TypeInferenceEngine inferenceEngine,
			SchemaGenerator schemaGenerator) {
		this.objectMapper = objectMapper;
		this.inferenceEngine = inferenceEngine;
		this.schemaGenerator = schemaGenerator;
	}

	public void convert(String inputJsonPath, String outputAvscPath) throws IOException {
		File inputFile = new File(inputJsonPath);
		if (!inputFile.exists()) {
			throw new IOException("Input JSON file not found: " + inputJsonPath);
		}

		JsonNode rootNode = objectMapper.readTree(inputFile);

		AvroTypeInfo typeInfo = inferenceEngine.inferType(rootNode, "Root");

		String prettyJson = schemaGenerator.generateSchemaJson(typeInfo, "RootRecord");

		writeSchemaToFile(prettyJson, outputAvscPath);
	}

	public Schema convertFromString(String jsonString, String recordName) throws IOException {
		JsonNode rootNode = objectMapper.readTree(jsonString);
		AvroTypeInfo typeInfo = inferenceEngine.inferType(rootNode, recordName);
		return schemaGenerator.generateSchema(typeInfo, recordName);
	}

	private void writeSchemaToFile(String schemaJson, String outputPath) throws IOException {
		File outputFile = new File(outputPath);
		File parentDir = outputFile.getParentFile();

		if (parentDir != null && !parentDir.exists()) {
			if (!parentDir.mkdirs()) {
				throw new IOException("Failed to create output directory: " + parentDir.getAbsolutePath());
			}
		}

		try (FileWriter writer = new FileWriter(outputFile)) {
			writer.write(schemaJson);
		}

		// Generate minified one-line version (.min.avsc)
		String minPath = buildMinPath(outputPath);
		String minified = minifyJson(schemaJson);
		try (FileWriter writer = new FileWriter(new File(minPath))) {
			writer.write(minified);
		}
	}

	private String buildMinPath(String outputPath) {
		int dotIndex = outputPath.lastIndexOf('.');
		if (dotIndex > 0) {
			return outputPath.substring(0, dotIndex) + ".min" + outputPath.substring(dotIndex);
		}
		return outputPath + ".min";
	}

	private String minifyJson(String json) {
		StringBuilder result = new StringBuilder();
		boolean inString = false;
		boolean escape = false;

		for (int i = 0; i < json.length(); i++) {
			char c = json.charAt(i);

			if (escape) {
				result.append(c);
				escape = false;
				continue;
			}

			if (c == '\\' && inString) {
				result.append(c);
				escape = true;
				continue;
			}

			if (c == '"') {
				inString = !inString;
				result.append(c);
				continue;
			}

			if (inString) {
				result.append(c);
			} else if (!Character.isWhitespace(c)) {
				result.append(c);
			}
		}

		return result.toString();
	}

	public SchemaGenerator getSchemaGenerator() {
		return schemaGenerator;
	}
}
