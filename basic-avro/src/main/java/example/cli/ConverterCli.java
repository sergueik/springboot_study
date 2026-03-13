package example.cli;

import example.converter.JsonToAvroConverter;
import example.converter.OpenApiToAvroConverter;
import example.serializer.AvroBinaryEncoder;
import example.serializer.AvroJsonGenerator;
import example.serializer.SchemaLoader;
import org.apache.avro.Schema;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ConverterCli {

	private final JsonToAvroConverter jsonConverter;
	private final OpenApiToAvroConverter openApiConverter;

	public ConverterCli() {
		this.jsonConverter = new JsonToAvroConverter();
		this.openApiConverter = new OpenApiToAvroConverter();
	}

	public ConverterCli(JsonToAvroConverter jsonConverter, OpenApiToAvroConverter openApiConverter) {
		this.jsonConverter = jsonConverter;
		this.openApiConverter = openApiConverter;
	}

	public int run(String[] args) {
		try {
			if (args != null && args.length > 0) {
				String command = args[0];
				if ("generate".equals(command)) {
					return runGenerate(args);
				}
				if ("encode".equals(command)) {
					return runEncode(args);
				}
			}

			return runConvert(args);

		} catch (IllegalArgumentException e) {
			System.err.println("Error: " + e.getMessage());
			System.err.println();
			printUsage();
			return 1;

		} catch (Exception e) {
			System.err.println("Error during operation: " + e.getMessage());
			e.printStackTrace();
			return 1;
		}
	}

	private int runGenerate(String[] args) throws Exception {
		if (args.length < 3) {
			throw new IllegalArgumentException("Usage: generate <schema.avsc> <output.json> [SchemaName]");
		}

		String schemaPath = args[1];
		String outputPath = args[2];
		String schemaName = args.length >= 4 ? args[3] : null;

		SchemaLoader loader = new SchemaLoader();
		Schema schema = loader.load(schemaPath, schemaName);

		System.out.println("Generating sample JSON from Avro schema...");
		System.out.println("  Schema: " + schemaPath);
		System.out.println("  Output: " + outputPath);
		if (schemaName != null) {
			System.out.println("  Record: " + schemaName);
		}

		AvroJsonGenerator generator = new AvroJsonGenerator();
		generator.generateToFile(schema, outputPath);

		System.out.println("JSON generation completed successfully!");
		return 0;
	}

	private int runEncode(String[] args) throws Exception {
		if (args.length < 4) {
			throw new IllegalArgumentException(
					"Usage: encode <schema.avsc> <input.json|--generate> <output.avro> [SchemaName]");
		}

		String schemaPath = args[1];
		String jsonInputOrFlag = args[2];
		String outputPath = args[3];
		String schemaName = args.length >= 5 ? args[4] : null;

		SchemaLoader loader = new SchemaLoader();
		Schema schema = loader.load(schemaPath, schemaName);

		boolean autoGenerate = "--generate".equals(jsonInputOrFlag);

		System.out.println("Encoding to Avro binary...");
		System.out.println("  Schema: " + schemaPath);
		System.out.println("  Output: " + outputPath);
		if (schemaName != null) {
			System.out.println("  Record: " + schemaName);
		}

		AvroBinaryEncoder encoder = new AvroBinaryEncoder();

		if (autoGenerate) {
			System.out.println("  Mode:   Auto-generate JSON then encode");
			AvroJsonGenerator generator = new AvroJsonGenerator();
			String json = generator.generate(schema);
			System.out.println("  Generated JSON:");
			System.out.println(json);
			encoder.encode(schema, json, outputPath);
		} else {
			System.out.println("  Input:  " + jsonInputOrFlag);
			encoder.encodeFromFile(schema, jsonInputOrFlag, outputPath);
		}

		System.out.println("Avro binary encoding completed successfully!");
		return 0;
	}

	private int runConvert(String[] args) throws Exception {
		CliArguments cliArgs = CliArguments.parse(args);
		cliArgs.validateInputExists();
		cliArgs.validateOutputWritable();

		String inputPath = cliArgs.getInputJsonPath();
		String outputPath = cliArgs.getOutputAvscPath();

		if (isOpenApiFile(inputPath)) {
			System.out.println("Converting OpenAPI/Swagger to Avro schema...");
			System.out.println("  Input:  " + inputPath);
			System.out.println("  Output: " + outputPath);

			// Check mode flags (4th argument)
			boolean registryMode = args.length >= 4 && "--registry".equals(args[3]);

			// If args contains a schema name (3rd argument), convert specific schema
			if (args.length >= 3 && !args[2].startsWith("--")) {
				String schemaName = args[2];
				System.out.println("  Schema: " + schemaName);

				if (registryMode) {
					System.out.println(
							"  Mode:   Registry (single self-contained schema for IBM/Confluent Schema Registry)");
					openApiConverter.convertRegistry(inputPath, schemaName, outputPath);
				} else {
					openApiConverter.convert(inputPath, schemaName, outputPath);
				}
			} else {
				// Extract output directory and convert all schemas
				int lastSlash = outputPath.lastIndexOf('/');
				if (lastSlash == -1) {
					lastSlash = outputPath.lastIndexOf('\\');
				}
				String outputDir = lastSlash > 0 ? outputPath.substring(0, lastSlash) : ".";
				System.out.println("  Generating all schemas to directory: " + outputDir);
				openApiConverter.convertAll(inputPath, outputDir);
			}
		} else {
			System.out.println("Converting JSON to Avro schema...");
			System.out.println("  Input:  " + inputPath);
			System.out.println("  Output: " + outputPath);
			jsonConverter.convert(inputPath, outputPath);
		}

		System.out.println("Conversion completed successfully!");
		return 0;
	}

	private boolean isOpenApiFile(String filePath) {
		// Check extension
		String lowerPath = filePath.toLowerCase();
		if (lowerPath.endsWith(".yaml") || lowerPath.endsWith(".yml")) {
			return true;
		}

		// Check file content for OpenAPI/Swagger markers
		try (BufferedReader reader = new BufferedReader(new FileReader(filePath))) {
			String line;
			int linesToCheck = 20;
			int linesChecked = 0;

			while ((line = reader.readLine()) != null && linesChecked < linesToCheck) {
				String trimmed = line.trim().toLowerCase();
				if (trimmed.startsWith("openapi:") || trimmed.startsWith("swagger:") || trimmed.contains("\"openapi\"")
						|| trimmed.contains("\"swagger\"")) {
					return true;
				}
				linesChecked++;
			}
		} catch (IOException e) {
			// If we can't read the file, assume it's JSON
			return false;
		}

		return false;
	}

	private void printUsage() {
		System.err.println("Usage:");
		System.err.println("  java -jar target/json-to-avro-converter.jar <command> [options]");
		System.err.println();
		System.err.println("Commands:");
		System.err.println("  generate  Generate sample JSON from an Avro schema");
		System.err.println("  encode    Encode JSON data to Avro binary format");
		System.err.println("  (none)    Convert JSON/OpenAPI to Avro schema (default)");
		System.err.println();
		System.err.println("Generate usage:");
		System.err.println("  generate <schema.avsc> <output.json> [SchemaName]");
		System.err.println();
		System.err.println("Encode usage:");
		System.err.println("  encode <schema.avsc> <input.json> <output.avro> [SchemaName]");
		System.err.println("  encode <schema.avsc> --generate <output.avro> [SchemaName]");
		System.err.println();
		System.err.println("Convert usage (default):");
		System.err.println("  <input-file> <output.avsc> [schema-name] [--unified]");
		System.err.println();
		System.err.println("Examples:");
		System.err.println("  # Generate sample JSON from Avro schema");
		System.err.println(
				"  java -jar target/json-to-avro-converter.jar generate src/main/avro/ResultResponse.avsc output.json ResultResponse");
		System.err.println();
		System.err.println("  # Encode JSON to Avro binary");
		System.err.println(
				"  java -jar target/json-to-avro-converter.jar encode src/main/avro/ResultResponse.avsc data.json output.avro ResultResponse");
		System.err.println();
		System.err.println("  # Auto-generate JSON and encode to Avro binary");
		System.err.println(
				"  java -jar target/json-to-avro-converter.jar encode src/main/avro/ResultResponse.avsc --generate output.avro ResultResponse");
		System.err.println();
		System.err.println("  # Convert JSON data to Avro schema");
		System.err.println("  java -jar target/json-to-avro-converter.jar data.json schema.avsc");
		System.err.println();
		System.err.println("  # Convert OpenAPI to registry-compatible Avro schema (IBM / Confluent Schema Registry)");
		System.err.println(
				"  java -jar target/json-to-avro-converter.jar api.yaml ResultResponse.avsc ResultResponse --registry");
	}
}
