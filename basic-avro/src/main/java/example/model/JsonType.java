package example.model;

/**
 * Enum representing the different types that can be found in JSON.
 *
 * This follows the Single Responsibility Principle by only defining type constants.
 */
public enum JsonType {
    NULL,
    BOOLEAN,
    INTEGER,
    LONG,
    FLOAT,
    DOUBLE,
    STRING,
    UUID,
    ENUM,
    ARRAY,
    OBJECT
}
