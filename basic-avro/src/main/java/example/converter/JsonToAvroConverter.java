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

/**
 * Main converter orchestrating JSON to Avro schema conversion.
 *
 * This class follows the Single Responsibility Principle by coordinating
 * between components rather than doing the work itself.
 * It also follows Dependency Inversion by depending on abstractions.
 */
public class JsonToAvroConverter {

    private final ObjectMapper objectMapper;
    private final TypeInferenceEngine inferenceEngine;
    private final SchemaGenerator schemaGenerator;

    /**
     * Constructor with default type detectors.
     */
    public JsonToAvroConverter() {
        this.objectMapper = new ObjectMapper();

        List<TypeDetector> detectors = Arrays.asList(
                new UuidDetector(),
                new EnumDetector()
        );

        this.inferenceEngine = new TypeInferenceEngine(detectors);
        this.schemaGenerator = new SchemaGenerator();
    }

    /**
     * Constructor with dependency injection for testing.
     *
     * @param objectMapper the JSON object mapper
     * @param inferenceEngine the type inference engine
     * @param schemaGenerator the schema generator
     */
    public JsonToAvroConverter(ObjectMapper objectMapper,
                               TypeInferenceEngine inferenceEngine,
                               SchemaGenerator schemaGenerator) {
        this.objectMapper = objectMapper;
        this.inferenceEngine = inferenceEngine;
        this.schemaGenerator = schemaGenerator;
    }

    /**
     * Convert a JSON file to an Avro schema file.
     *
     * @param inputJsonPath path to input JSON file
     * @param outputAvscPath path to output AVSC file
     * @throws IOException if file operations fail
     */
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

    /**
     * Convert a JSON string to an Avro schema.
     *
     * @param jsonString the JSON string
     * @param recordName the name for the root record
     * @return the generated Avro schema
     * @throws IOException if JSON parsing fails
     */
    public Schema convertFromString(String jsonString, String recordName) throws IOException {
        JsonNode rootNode = objectMapper.readTree(jsonString);
        AvroTypeInfo typeInfo = inferenceEngine.inferType(rootNode, recordName);
        return schemaGenerator.generateSchema(typeInfo, recordName);
    }

    /**
     * Write the schema to a file.
     */
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

    /**
     * Build the .min.avsc path from the original output path.
     */
    private String buildMinPath(String outputPath) {
        int dotIndex = outputPath.lastIndexOf('.');
        if (dotIndex > 0) {
            return outputPath.substring(0, dotIndex) + ".min" + outputPath.substring(dotIndex);
        }
        return outputPath + ".min";
    }

    /**
     * Minify a JSON string by removing unnecessary whitespace.
     */
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

    /**
     * Get the schema generator (for testing).
     */
    public SchemaGenerator getSchemaGenerator() {
        return schemaGenerator;
    }
}
