package example.util;

import com.fasterxml.jackson.databind.JsonNode;
import example.converter.interfaces.TypeDetector;

import java.util.regex.Pattern;

/**
 * Detects ENUM values using heuristic pattern matching.
 *
 * This class implements TypeDetector and follows the Single Responsibility Principle
 * by focusing solely on ENUM detection.
 *
 * Heuristic: UPPER_CASE with or without underscores
 * Examples with underscores: STATUS_ACTIVE, USER_ROLE_ADMIN, ERROR_CODE_404
 * Examples without underscores: SUCCESS, ERROR, ACTIVE, PENDING, OK
 * Pattern: ^[A-Z][A-Z0-9]*(_[A-Z0-9]+)*$
 */
public class EnumDetector implements TypeDetector {

    private static final Pattern ENUM_PATTERN = Pattern.compile(
            "^[A-Z][A-Z0-9]*(_[A-Z0-9]+)*$"
    );

    private static final int MIN_LENGTH = 2;

    @Override
    public boolean matches(String value) {
        if (value == null || value.length() < MIN_LENGTH) {
            return false;
        }
        return ENUM_PATTERN.matcher(value).matches();
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
        return null;
    }

    @Override
    public int getPriority() {
        return 50;
    }
}
