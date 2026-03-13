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

/**
 * Converter for transforming OpenAPI/Swagger specifications to Avro schemas.
 *
 * This class follows the Single Responsibility Principle by coordinating
 * between the parser, mapper, and schema generator components.
 * It also follows Dependency Inversion by depending on abstractions.
 */
public class OpenApiToAvroConverter {

    private final OpenApiParser parser;
    private final SchemaGenerator schemaGenerator;
    private final RegistrySchemaGenerator registrySchemaGenerator;

    /**
     * Constructor with default components.
     */
    public OpenApiToAvroConverter() {
        this.parser = new OpenApiParser();
        this.schemaGenerator = new SchemaGenerator();
        this.registrySchemaGenerator = new RegistrySchemaGenerator();
    }

    /**
     * Constructor with dependency injection for testing.
     *
     * @param parser          the OpenAPI parser
     * @param schemaGenerator the schema generator
     */
    public OpenApiToAvroConverter(OpenApiParser parser, SchemaGenerator schemaGenerator) {
        this.parser = parser;
        this.schemaGenerator = schemaGenerator;
        this.registrySchemaGenerator = new RegistrySchemaGenerator();
    }

    /**
     * Convert an OpenAPI file to Avro schema files.
     * Generates one Avro schema file per schema defined in components/schemas.
     *
     * @param inputOpenApiPath path to input OpenAPI file (YAML or JSON)
     * @param outputDirectory  directory where Avro schema files will be written
     * @throws IOException if file operations fail
     */
    public void convertAll(String inputOpenApiPath, String outputDirectory) throws IOException {
        OpenAPI openAPI = parser.parse(inputOpenApiPath);

        if (openAPI.getComponents() == null ||
                openAPI.getComponents().getSchemas() == null ||
                openAPI.getComponents().getSchemas().isEmpty()) {
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

    /**
     * Convert a specific schema from an OpenAPI file to an Avro schema file.
     *
     * @param inputOpenApiPath path to input OpenAPI file (YAML or JSON)
     * @param schemaName       name of the schema in components/schemas to convert
     * @param outputAvscPath   path to output AVSC file
     * @throws IOException if file operations fail
     */
    public void convert(String inputOpenApiPath, String schemaName, String outputAvscPath) throws IOException {
        OpenAPI openAPI = parser.parse(inputOpenApiPath);

        if (openAPI.getComponents() == null ||
                openAPI.getComponents().getSchemas() == null) {
            throw new IOException("No schemas found in OpenAPI specification");
        }

        Schema<?> schema = openAPI.getComponents().getSchemas().get(schemaName);
        if (schema == null) {
            throw new IOException("Schema '" + schemaName + "' not found in OpenAPI specification. " +
                    "Available schemas: " + openAPI.getComponents().getSchemas().keySet());
        }

        OpenApiToAvroTypeMapper mapper = new OpenApiToAvroTypeMapper(openAPI);
        AvroTypeInfo typeInfo = mapper.mapSchema(schema, schemaName);

        String schemaJson = schemaGenerator.generateSchemaJson(typeInfo, schemaName);
        writeSchemaToFile(schemaJson, outputAvscPath);
    }

    /**
     * Convert a specific schema from an OpenAPI file to an IBM Schema Registry compatible Avro schema file.
     * Produces a single self-contained JSON object with all nested types embedded inline.
     *
     * @param inputOpenApiPath path to input OpenAPI file (YAML or JSON)
     * @param schemaName       name of the schema in components/schemas to convert
     * @param outputAvscPath   path to output AVSC file
     * @throws IOException if file operations fail
     */
    public void convertRegistry(String inputOpenApiPath, String schemaName, String outputAvscPath) throws IOException {
        OpenAPI openAPI = parser.parse(inputOpenApiPath);

        if (openAPI.getComponents() == null ||
                openAPI.getComponents().getSchemas() == null) {
            throw new IOException("No schemas found in OpenAPI specification");
        }

        Schema<?> schema = openAPI.getComponents().getSchemas().get(schemaName);
        if (schema == null) {
            throw new IOException("Schema '" + schemaName + "' not found in OpenAPI specification. " +
                    "Available schemas: " + openAPI.getComponents().getSchemas().keySet());
        }

        OpenApiToAvroTypeMapper mapper = new OpenApiToAvroTypeMapper(openAPI);
        AvroTypeInfo typeInfo = mapper.mapSchema(schema, schemaName);

        String schemaJson = registrySchemaGenerator.generateRegistrySchema(typeInfo, schemaName);
        writeSchemaToFile(schemaJson, outputAvscPath);
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
