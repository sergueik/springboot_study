package example;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.avro.Schema;

import example.serializer.AvroBinaryEncoder;
import example.serializer.AvroJsonGenerator;
import example.serializer.SchemaLoader;
import example.converter.JsonToAvroConverter;
import example.converter.OpenApiToAvroConverter;

public class App {

	private static boolean debug = false;

	private static Schema schema = null;
	private static final SchemaLoader loader = new SchemaLoader();
	private static final AvroBinaryEncoder encoder = new AvroBinaryEncoder();
	private static final AvroJsonGenerator generator = new AvroJsonGenerator();
	private static final OpenApiToAvroConverter openApiConverter = new OpenApiToAvroConverter();
	private static final JsonToAvroConverter jsonConverter = new JsonToAvroConverter();

	public static void main(String[] args) throws Exception {
		Map<String, String> cli = parseArgs(args);

		String schemaPath = null;
		String outputPath = null;
		String inputPath = null;
		String schemaName = null;
		String data = null;
		String operation = null;
		boolean autoGenerate = false;
		boolean unified = false;
		boolean registryMode = false;

		Long threshold = 0L;
		int exitCode = 0;

		if (cli.containsKey("debug"))
			debug = Boolean.parseBoolean(cli.get("debug"));

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("inputfile"))
			inputPath = cli.get("inputfile");
		if (cli.containsKey("outputfile"))
			outputPath = cli.get("outputfile");
		if (cli.containsKey("schema"))
			schemaName = cli.get("schema");
		if (cli.containsKey("schemafile"))
			schemaPath = cli.get("schemafile");
		if (cli.containsKey("threshold"))
			threshold = Long.parseLong(cli.get("threshold"));

		if (cli.containsKey("auto"))
			autoGenerate = Boolean.parseBoolean(cli.get("auto"));
		if (cli.containsKey("unified"))
			unified = Boolean.parseBoolean(cli.get("unified"));
		if (cli.containsKey("registry"))
			registryMode = Boolean.parseBoolean(cli.get("registry"));
		if (cli.containsKey("operation"))
			operation = cli.get("operation");

		if (operation == null) {
			System.err.println("Missing required argument: operation");
			System.err.println("Usage:");
			System.err.println(" jar -operation <command> [options]");
			System.err.println("operations:");
			System.err.println("\tgenerate\tGenerate sample JSON from an Avro schema");
			System.err.println("\tencode\tEncode JSON data to Avro binary format");
			System.err.println("\tconvert\tConvert JSON/OpenAPI to Avro schema (default)");
			return;
		}

		schema = loader.load(schemaPath, schemaName);

		if ("generate".equals(operation)) {
			exitCode = runGenerate(outputPath, schemaPath, schemaName);
		}
		if ("encode".equals(operation)) {
			exitCode = runEncode(outputPath, inputPath, schemaPath, schemaName, autoGenerate);
		}
		if ("convert".equals(operation)) {
			exitCode = runConvert(inputPath, outputPath, schemaPath, schemaName, registryMode);
		}

		// ConverterCli converterCli = new ConverterCli();
		// int exitCode = converterCli.run(args);
		if (debug) {
			System.err.println("Done: " + operation);
		}
		System.exit(exitCode);
	}

	private static int runGenerate(final String outputPath, final String schemaPath, final String schemaName)
			throws Exception {

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

	private static int runEncode(final String outputPath, final String inputPath, final String schemaPath,
			final String schemaName, final boolean autoGenerate) throws Exception {

		SchemaLoader loader = new SchemaLoader();
		Schema schema = loader.load(schemaPath, schemaName);

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
			System.out.println("  Input:  " + inputPath /* jsonInputOrFlag */);
			encoder.encodeFromFile(schema, inputPath, outputPath);
		}

		System.out.println("Avro binary encoding completed successfully!");
		return 0;
	}

	private static int runConvert(final String inputPath, final String outputPath, final String schemaPath,
			final String schemaName, final boolean registryMode) throws Exception {

		if (isOpenApiFile(inputPath)) {
			System.out.println("Converting OpenAPI/Swagger to Avro schema...");
			System.out.println("  Input:  " + inputPath);
			System.out.println("  Output: " + outputPath);

			// If args contains a schema name (3rd argument), convert specific schema
			if (schemaName != null) {
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

	private static boolean isOpenApiFile(String filePath) {
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

	private static void printUsage() {
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

	private static Map<String, String> parseArgs(String[] args) {
		if (Arrays.asList(args).contains("debug"))
			System.err.println("Processing: " + Arrays.asList(args));
		Map<String, String> map = new HashMap<>();
		for (int i = 0; i < args.length - 1; i++) {
			if (args[i].startsWith("-")) {
				map.put(args[i].substring(1), args[i + 1]);
				i++;
			}
		}
		return map;
	}

}
