package example.converter.interfaces;

import com.fasterxml.jackson.databind.JsonNode;

public interface TypeDetector {

	boolean matches(String value);

	boolean matchesArray(JsonNode arrayNode);

	String getLogicalType();

	default int getPriority() {
		return 0;
	}
}
