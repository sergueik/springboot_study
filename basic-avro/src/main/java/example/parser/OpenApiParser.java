package example.parser;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;

import java.io.File;
import java.io.IOException;
import java.util.List;

/**
 * Parser for OpenAPI/Swagger specification files (YAML or JSON).
 *
 * This class follows the Single Responsibility Principle by focusing solely
 * on parsing OpenAPI specifications and validating them.
 */
public class OpenApiParser {

    private final OpenAPIV3Parser parser;
    private final ParseOptions parseOptions;

    /**
     * Constructor with default parse options.
     */
    public OpenApiParser() {
        this.parser = new OpenAPIV3Parser();
        this.parseOptions = new ParseOptions();
        this.parseOptions.setResolve(true);
        this.parseOptions.setResolveFully(true);
    }

    /**
     * Constructor with custom parse options for testing.
     *
     * @param parseOptions custom parse options
     */
    public OpenApiParser(ParseOptions parseOptions) {
        this.parser = new OpenAPIV3Parser();
        this.parseOptions = parseOptions;
    }

    /**
     * Parse an OpenAPI specification file (YAML or JSON).
     *
     * @param filePath path to the OpenAPI file
     * @return parsed OpenAPI object
     * @throws IOException if the file cannot be read or parsed
     */
    public OpenAPI parse(String filePath) throws IOException {
        File file = new File(filePath);
        if (!file.exists()) {
            throw new IOException("OpenAPI file not found: " + filePath);
        }

        if (!file.isFile()) {
            throw new IOException("Path is not a file: " + filePath);
        }

        SwaggerParseResult result = parser.readLocation(filePath, null, parseOptions);

        if (result.getOpenAPI() == null) {
            String errorMessages = formatErrors(result.getMessages());
            throw new IOException("Failed to parse OpenAPI file: " + errorMessages);
        }

        if (result.getMessages() != null && !result.getMessages().isEmpty()) {
            System.err.println("Warnings while parsing OpenAPI file:");
            result.getMessages().forEach(msg -> System.err.println("  - " + msg));
        }

        return result.getOpenAPI();
    }

    /**
     * Parse an OpenAPI specification from a string (YAML or JSON).
     *
     * @param content the OpenAPI specification content
     * @return parsed OpenAPI object
     * @throws IOException if the content cannot be parsed
     */
    public OpenAPI parseFromString(String content) throws IOException {
        if (content == null || content.trim().isEmpty()) {
            throw new IOException("OpenAPI content is null or empty");
        }

        SwaggerParseResult result = parser.readContents(content, null, parseOptions);

        if (result.getOpenAPI() == null) {
            String errorMessages = formatErrors(result.getMessages());
            throw new IOException("Failed to parse OpenAPI content: " + errorMessages);
        }

        return result.getOpenAPI();
    }

    /**
     * Format error messages from parse result.
     */
    private String formatErrors(List<String> messages) {
        if (messages == null || messages.isEmpty()) {
            return "Unknown error";
        }
        return String.join("; ", messages);
    }
}
