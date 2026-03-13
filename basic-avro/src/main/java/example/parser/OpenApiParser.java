package example.parser;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.parser.OpenAPIV3Parser;
import io.swagger.v3.parser.core.models.ParseOptions;
import io.swagger.v3.parser.core.models.SwaggerParseResult;

import java.io.File;
import java.io.IOException;
import java.util.List;

public class OpenApiParser {

	private final OpenAPIV3Parser parser;
	private final ParseOptions parseOptions;

	public OpenApiParser() {
		this.parser = new OpenAPIV3Parser();
		this.parseOptions = new ParseOptions();
		this.parseOptions.setResolve(true);
		this.parseOptions.setResolveFully(true);
	}

	public OpenApiParser(ParseOptions parseOptions) {
		this.parser = new OpenAPIV3Parser();
		this.parseOptions = parseOptions;
	}

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

	private String formatErrors(List<String> messages) {
		return messages == null || messages.isEmpty() ? "Unknown error" : String.join("; ", messages);
	}
}
