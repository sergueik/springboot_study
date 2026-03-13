package example.converter;

import example.mapper.OpenApiToAvroTypeMapper;
import example.model.AvroTypeInfo;
import example.parser.OpenApiParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.avro.Schema.Type;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Map;

public class OpenApiToAvroConverter {

	private final OpenApiParser parser;
	private final SchemaGenerator schemaGenerator;
	private final RegistrySchemaGenerator registrySchemaGenerator;

	public OpenApiToAvroConverter() {
		this.parser = new OpenApiParser();
		this.schemaGenerator = new SchemaGenerator();
		this.registrySchemaGenerator = new RegistrySchemaGenerator();
	}

	public OpenApiToAvroConverter(OpenApiParser parser, SchemaGenerator schemaGenerator) {
		this.parser = parser;
		this.schemaGenerator = schemaGenerator;
		this.registrySchemaGenerator = new RegistrySchemaGenerator();
	}

	public void convertAll(String inputOpenApiPath, String outputDirectory) throws IOException {
		OpenAPI openAPI = parser.parse(inputOpenApiPath);

		if (openAPI.getComponents() == null || openAPI.getComponents().getSchemas() == null
				|| openAPI.getComponents().getSchemas().isEmpty()) {
			throw new IOException("No schemas found in OpenAPI specification");
		}

		File outputDir = new File(outputDirectory);
		if (!outputDir.exists()) {
			if (!outputDir.mkdirs()) {
				throw new IOException("Failed to create output directory: " + outputDirectory);
			}
		}

		OpenApiToAvroTypeMapper mapper = new OpenApiToAvroTypeMapper(openAPI);

		for (Map.Entry<String, Schema> entry : openAPI.getComponents().getSchemas().entrySet()) {
			String schemaName = entry.getKey();
			Schema<?> schema = entry.getValue();

			AvroTypeInfo typeInfo = mapper.mapSchema(schema, schemaName);

			// Only generate files for record types (not simple enums)
			if (typeInfo.getAvroType() == Type.RECORD) {
				String outputFileName = schemaName + ".avsc";
				String outputPath = new File(outputDir, outputFileName).getPath();

				String schemaJson = schemaGenerator.generateSchemaJson(typeInfo, schemaName);
				writeSchemaToFile(schemaJson, outputPath);

				System.out.println("Generated: " + outputFileName);
			}
		}
	}

	public void convert(String inputOpenApiPath, String schemaName, String outputAvscPath) throws IOException {
		OpenAPI openAPI = parser.parse(inputOpenApiPath);

		if (openAPI.getComponents() == null || openAPI.getComponents().getSchemas() == null) {
			throw new IOException("No schemas found in OpenAPI specification");
		}

		Schema<?> schema = openAPI.getComponents().getSchemas().get(schemaName);
		if (schema == null) {
			throw new IOException("Schema '" + schemaName + "' not found in OpenAPI specification. "
					+ "Available schemas: " + openAPI.getComponents().getSchemas().keySet());
		}

		OpenApiToAvroTypeMapper mapper = new OpenApiToAvroTypeMapper(openAPI);
		AvroTypeInfo typeInfo = mapper.mapSchema(schema, schemaName);

		String schemaJson = schemaGenerator.generateSchemaJson(typeInfo, schemaName);
		writeSchemaToFile(schemaJson, outputAvscPath);
	}

	public void convertRegistry(String inputOpenApiPath, String schemaName, String outputAvscPath) throws IOException {
		OpenAPI openAPI = parser.parse(inputOpenApiPath);

		if (openAPI.getComponents() == null || openAPI.getComponents().getSchemas() == null) {
			throw new IOException("No schemas found in OpenAPI specification");
		}

		Schema<?> schema = openAPI.getComponents().getSchemas().get(schemaName);
		if (schema == null) {
			throw new IOException("Schema '" + schemaName + "' not found in OpenAPI specification. "
					+ "Available schemas: " + openAPI.getComponents().getSchemas().keySet());
		}

		OpenApiToAvroTypeMapper mapper = new OpenApiToAvroTypeMapper(openAPI);
		AvroTypeInfo typeInfo = mapper.mapSchema(schema, schemaName);

		String schemaJson = registrySchemaGenerator.generateRegistrySchema(typeInfo, schemaName);
		writeSchemaToFile(schemaJson, outputAvscPath);
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
