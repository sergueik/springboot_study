package example.converter.interfaces;

import com.fasterxml.jackson.databind.JsonNode;

/**
 * Interface for detecting special types in JSON values.
 *
 * This interface follows the Open/Closed and Dependency Inversion principles,
 * allowing new type detectors to be added without modifying existing code.
 */
public interface TypeDetector {

    /**
     * Checks if a single string value matches this type pattern.
     *
     * @param value the string value to check
     * @return true if the value matches this type, false otherwise
     */
    boolean matches(String value);

    /**
     * Checks if all values in a JSON array match this type pattern.
     *
     * @param arrayNode the JSON array node to check
     * @return true if all non-null elements match this type, false otherwise
     */
    boolean matchesArray(JsonNode arrayNode);

    /**
     * Returns the logical type name for Avro schema.
     *
     * @return the logical type name (e.g., "uuid") or null if not a logical type
     */
    String getLogicalType();

    /**
     * Returns the priority of this detector.
     * Higher priority detectors are checked first.
     *
     * @return the priority (higher values = higher priority)
     */
    default int getPriority() {
        return 0;
    }
}
