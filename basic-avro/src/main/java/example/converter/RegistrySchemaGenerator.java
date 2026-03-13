package example.converter;

import example.model.AvroTypeInfo;
import org.apache.avro.Schema;

import java.util.*;

/**
 * Generates Avro schemas compatible with schema registries (e.g. IBM Schema Registry / Apicurio).
 *
 * Produces a single self-contained JSON object (not an array).
 * Nested named types (records, enums) are embedded inline at their first occurrence.
 * Subsequent references to the same named type use the full qualified name string.
 *
 * This format is required by IBM Schema Registry and Confluent Schema Registry.
 */
public class RegistrySchemaGenerator {

    private static final String DEFAULT_NAMESPACE = "example.generated";

    private final Set<String> definedTypes = new LinkedHashSet<>();

    /**
     * Generate an IBM Schema Registry compatible Avro schema.
     *
     * @param rootType the root type information
     * @param rootName the name for the root record
     * @return single JSON object string (not a JSON array)
     */
    public String generateRegistrySchema(AvroTypeInfo rootType, String rootName) {
        definedTypes.clear();
        // Pre-register the root to prevent infinite recursion on self-referencing types
        definedTypes.add(DEFAULT_NAMESPACE + "." + rootName);
        return inlineRecord(rootType, rootName, DEFAULT_NAMESPACE, 0);
    }

    // -------------------------------------------------------------------------
    // Inline type builders
    // -------------------------------------------------------------------------

    /**
     * Build an inline record definition.
     * Opening '{' has no leading indent (caller places it after "type": or at start of output).
     * Content is at indent(keyIndent + 1), closing '}' at indent(keyIndent).
     */
    private String inlineRecord(AvroTypeInfo typeInfo, String name, String namespace, int keyIndent) {
        String i1 = ind(keyIndent + 1);
        String fi = ind(keyIndent + 2); // field object {
        String fc = ind(keyIndent + 3); // field content (name, type, default)

        StringBuilder sb = new StringBuilder("{\n");
        sb.append(i1).append("\"type\": \"record\",\n");
        sb.append(i1).append("\"name\": \"").append(name).append("\",\n");
        sb.append(i1).append("\"namespace\": \"").append(namespace).append("\"");

        sb.append(",\n").append(i1).append("\"fields\": [\n");

        if (typeInfo.getFields() != null) {
            List<Map.Entry<String, AvroTypeInfo>> fields = new ArrayList<>(typeInfo.getFields().entrySet());
            for (int i = 0; i < fields.size(); i++) {
                Map.Entry<String, AvroTypeInfo> field = fields.get(i);
                AvroTypeInfo fieldType = field.getValue();

                sb.append(fi).append("{\n");
                sb.append(fc).append("\"name\": \"").append(field.getKey()).append("\"");

                sb.append(",\n").append(fc).append("\"type\": ");
                sb.append(fieldType(fieldType, namespace, keyIndent + 3));

                if (isNullableUnion(fieldType)) {
                    sb.append(",\n").append(fc).append("\"default\": null\n");
                } else {
                    sb.append("\n");
                }

                sb.append(fi).append("}");
                if (i < fields.size() - 1) sb.append(",");
                sb.append("\n");
            }
        }

        sb.append(i1).append("]\n");
        sb.append(ind(keyIndent)).append("}");
        return sb.toString();
    }

    /**
     * Build an inline enum definition.
     */
    private String inlineEnum(AvroTypeInfo typeInfo, String name, String namespace, int keyIndent) {
        String i1 = ind(keyIndent + 1);

        StringBuilder sb = new StringBuilder("{\n");
        sb.append(i1).append("\"type\": \"enum\",\n");
        sb.append(i1).append("\"name\": \"").append(name).append("\",\n");
        sb.append(i1).append("\"namespace\": \"").append(namespace).append("\"");

        sb.append(",\n").append(i1).append("\"symbols\": [");

        List<String> symbols = typeInfo.getEnumSymbols();
        if (symbols != null) {
            for (int i = 0; i < symbols.size(); i++) {
                sb.append("\"").append(symbols.get(i)).append("\"");
                if (i < symbols.size() - 1) sb.append(", ");
            }
        }

        sb.append("]\n");
        sb.append(ind(keyIndent)).append("}");
        return sb.toString();
    }

    // -------------------------------------------------------------------------
    // Field type resolver
    // -------------------------------------------------------------------------

    /**
     * Generate the type JSON for a field.
     *
     * The returned string starts immediately (no leading whitespace) and is placed
     * right after "type": by the caller.
     *
     * For named types (record/enum):
     *   - First occurrence → full inline definition (may be multiline)
     *   - Subsequent occurrences → quoted full name reference
     *
     * @param typeInfo   the type information
     * @param namespace  the current namespace
     * @param keyIndent  the indent level of the line containing the "type": key
     */
    private String fieldType(AvroTypeInfo typeInfo, String namespace, int keyIndent) {
        switch (typeInfo.getAvroType()) {
            case NULL:    return "\"null\"";
            case BOOLEAN: return "\"boolean\"";
            case INT:     return "\"int\"";
            case LONG:
                if ("timestamp-millis".equals(typeInfo.getLogicalType())) {
                    return "{\"type\": \"long\", \"logicalType\": \"timestamp-millis\"}";
                }
                return "\"long\"";
            case FLOAT:   return "\"float\"";
            case DOUBLE:  return "\"double\"";
            case STRING:  return stringType(typeInfo);

            case ENUM: {
                String name = typeInfo.getRecordName() != null ? typeInfo.getRecordName() : "UnknownEnum";
                String fullName = namespace + "." + name;
                if (definedTypes.contains(fullName)) {
                    return "\"" + fullName + "\"";
                }
                definedTypes.add(fullName);
                return inlineEnum(typeInfo, name, namespace, keyIndent);
            }

            case RECORD: {
                String name = typeInfo.getRecordName() != null ? typeInfo.getRecordName() : "UnknownRecord";
                String fullName = namespace + "." + name;
                if (definedTypes.contains(fullName)) {
                    return "\"" + fullName + "\"";
                }
                definedTypes.add(fullName);
                return inlineRecord(typeInfo, name, namespace, keyIndent);
            }

            case ARRAY: {
                AvroTypeInfo itemType = typeInfo.getArrayItemType();
                if (itemType == null) {
                    return "{\"type\": \"array\", \"items\": \"string\"}";
                }
                if (isSimple(itemType)) {
                    return "{\"type\": \"array\", \"items\": " + fieldType(itemType, namespace, keyIndent) + "}";
                }
                // Multiline array for complex item types
                String i1 = ind(keyIndent + 1);
                StringBuilder sb = new StringBuilder("{\n");
                sb.append(i1).append("\"type\": \"array\",\n");
                sb.append(i1).append("\"items\": ");
                sb.append(fieldType(itemType, namespace, keyIndent + 1));
                sb.append("\n").append(ind(keyIndent)).append("}");
                return sb.toString();
            }

            case UNION: {
                List<AvroTypeInfo> unionTypes = typeInfo.getUnionTypes();
                if (unionTypes == null || unionTypes.isEmpty()) {
                    return "\"null\"";
                }
                StringBuilder sb = new StringBuilder("[");
                for (int i = 0; i < unionTypes.size(); i++) {
                    if (i > 0) sb.append(", ");
                    sb.append(fieldType(unionTypes.get(i), namespace, keyIndent));
                }
                sb.append("]");
                return sb.toString();
            }

            default:
                return "\"string\"";
        }
    }

    // -------------------------------------------------------------------------
    // Helpers
    // -------------------------------------------------------------------------

    private String stringType(AvroTypeInfo typeInfo) {
        boolean hasLogical = "uuid".equals(typeInfo.getLogicalType());
        boolean hasPattern = typeInfo.getPattern() != null && !typeInfo.getPattern().isEmpty();

        if (!hasLogical && !hasPattern) {
            return "\"string\"";
        }

        StringBuilder sb = new StringBuilder("{\"type\": \"string\"");
        if (hasLogical) {
            sb.append(", \"logicalType\": \"uuid\"");
        }
        if (hasPattern) {
            String escaped = typeInfo.getPattern()
                    .replace("\\", "\\\\")
                    .replace("\"", "\\\"");
            sb.append(", \"pattern\": \"").append(escaped).append("\"");
        }
        sb.append("}");
        return sb.toString();
    }

    private boolean isNullableUnion(AvroTypeInfo typeInfo) {
        return typeInfo.getAvroType() == Schema.Type.UNION
                && typeInfo.getUnionTypes() != null
                && !typeInfo.getUnionTypes().isEmpty()
                && typeInfo.getUnionTypes().get(0).getAvroType() == Schema.Type.NULL;
    }

    /** Returns true if the type generates a compact single-line JSON value. */
    private boolean isSimple(AvroTypeInfo typeInfo) {
        switch (typeInfo.getAvroType()) {
            case NULL:
            case BOOLEAN:
            case INT:
            case LONG:
            case FLOAT:
            case DOUBLE:
            case STRING:
                return true;
            default:
                return false;
        }
    }

    /** Returns an indentation string of 2 × level spaces. */
    private static String ind(int level) {
        return "  ".repeat(Math.max(0, level));
    }

}
