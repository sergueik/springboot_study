package example.util;

import com.fasterxml.jackson.databind.JsonNode;
import example.converter.interfaces.TypeDetector;

import java.util.regex.Pattern;

public class UuidDetector implements TypeDetector {

	private static final Pattern UUID_PATTERN = Pattern
			.compile("^[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}$");

	@Override
	public boolean matches(String value) {
		if (value == null || value.isEmpty()) {
			return false;
		}
		return UUID_PATTERN.matcher(value).matches();
	}

	@Override
	public boolean matchesArray(JsonNode arrayNode) {
		if (arrayNode == null || !arrayNode.isArray() || arrayNode.isEmpty()) {
			return false;
		}

		int validCount = 0;
		int totalNonNull = 0;

		for (JsonNode element : arrayNode) {
			if (!element.isNull()) {
				totalNonNull++;
				if (element.isTextual() && matches(element.asText())) {
					validCount++;
				}
			}
		}

		return totalNonNull > 0 && validCount == totalNonNull;
	}

	@Override
	public String getLogicalType() {
		return "uuid";
	}

	@Override
	public int getPriority() {
		return 100;
	}
}
