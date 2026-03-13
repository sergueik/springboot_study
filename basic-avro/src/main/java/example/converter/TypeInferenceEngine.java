package example.converter;

import com.fasterxml.jackson.databind.JsonNode;
import example.converter.interfaces.TypeDetector;
import example.model.AvroTypeInfo;
import org.apache.avro.Schema;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Core engine for inferring Avro types from JSON data.
 *
 * This class follows the Dependency Inversion Principle by depending on the
 * TypeDetector abstraction rather than concrete implementations.
 * It also follows the Single Responsibility Principle by focusing solely on
 * type inference.
 */
public class TypeInferenceEngine {

    private final List<TypeDetector> typeDetectors;

    /**
     * Constructor with dependency injection.
     *
     * @param typeDetectors list of type detectors (e.g., UUID, ENUM)
     */
    public TypeInferenceEngine(List<TypeDetector> typeDetectors) {
        this.typeDetectors = typeDetectors != null ? new ArrayList<>(typeDetectors) : new ArrayList<>();
        this.typeDetectors.sort((d1, d2) -> Integer.compare(d2.getPriority(), d1.getPriority()));
    }

    /**
     * Infer the Avro type from a JSON node.
     *
     * @param node      the JSON node to analyze
     * @param fieldName the field name (for record generation)
     * @return the inferred Avro type information
     */
    public AvroTypeInfo inferType(JsonNode node, String fieldName) {
        if (node == null || node.isNull()) {
            return createNullableType(Schema.Type.STRING);
        }

        if (node.isBoolean()) {
            return AvroTypeInfo.builder()
                    .avroType(Schema.Type.BOOLEAN)
                    .build();
        }

        if (node.isNumber()) {
            // Traiter les nombres comme des strings
            return AvroTypeInfo.builder()
                    .avroType(Schema.Type.STRING)
                    .build();
        }

        if (node.isTextual()) {
            return inferStringType(node.asText(), fieldName);
        }

        if (node.isArray()) {
            return inferArrayType(node, fieldName);
        }

        if (node.isObject()) {
            return inferRecordType(node, fieldName);
        }

        return AvroTypeInfo.builder()
                .avroType(Schema.Type.STRING)
                .build();
    }

    /**
     * Infer string type, detecting UUID and ENUM patterns.
     */
    private AvroTypeInfo inferStringType(String value, String fieldName) {
        for (TypeDetector detector : typeDetectors) {
            if (detector.matches(value)) {
                if (detector.getLogicalType() != null) {
                    return AvroTypeInfo.builder()
                            .avroType(Schema.Type.STRING)
                            .logicalType(detector.getLogicalType())
                            .recordName(capitalize(fieldName))
                            .build();
                }
            }
        }

        return AvroTypeInfo.builder()
                .avroType(Schema.Type.STRING)
                .build();
    }

    /**
     * Infer array type by analyzing all elements.
     */
    private AvroTypeInfo inferArrayType(JsonNode arrayNode, String fieldName) {
        if (arrayNode.isEmpty()) {
            return AvroTypeInfo.builder()
                    .avroType(Schema.Type.ARRAY)
                    .arrayItemType(AvroTypeInfo.builder()
                            .avroType(Schema.Type.STRING)
                            .build())
                    .build();
        }

        for (TypeDetector detector : typeDetectors) {
            if (detector.matchesArray(arrayNode)) {
                if ("uuid".equals(detector.getLogicalType())) {
                    return AvroTypeInfo.builder()
                            .avroType(Schema.Type.ARRAY)
                            .arrayItemType(AvroTypeInfo.builder()
                                    .avroType(Schema.Type.STRING)
                                    .logicalType("uuid")
                                    .build())
                            .build();
                } else {
                    List<String> symbols = extractEnumSymbols(arrayNode);
                    return AvroTypeInfo.builder()
                            .avroType(Schema.Type.ARRAY)
                            .arrayItemType(AvroTypeInfo.builder()
                                    .avroType(Schema.Type.ENUM)
                                    .enumSymbols(symbols)
                                    .recordName(capitalize(fieldName))
                                    .build())
                            .build();
                }
            }
        }

        List<AvroTypeInfo> types = new ArrayList<>();
        boolean hasNull = false;

        for (JsonNode element : arrayNode) {
            if (element.isNull()) {
                hasNull = true;
            } else {
                AvroTypeInfo elementType = inferType(element, fieldName);
                if (!containsType(types, elementType)) {
                    types.add(elementType);
                }
            }
        }

        if (types.isEmpty()) {
            return AvroTypeInfo.builder()
                    .avroType(Schema.Type.ARRAY)
                    .arrayItemType(createNullableType(Schema.Type.STRING))
                    .build();
        }

        AvroTypeInfo itemType;
        if (types.size() == 1) {
            itemType = types.get(0);
            if (hasNull) {
                itemType = makeNullable(itemType);
            }
        } else {
            AvroTypeInfo.Builder unionBuilder = AvroTypeInfo.builder()
                    .avroType(Schema.Type.UNION);
            if (hasNull) {
                unionBuilder.addUnionType(AvroTypeInfo.builder()
                        .avroType(Schema.Type.NULL)
                        .build());
            }
            types.forEach(unionBuilder::addUnionType);
            itemType = unionBuilder.build();
        }

        return AvroTypeInfo.builder()
                .avroType(Schema.Type.ARRAY)
                .arrayItemType(itemType)
                .build();
    }

    /**
     * Extract all unique enum symbols from array.
     */
    private List<String> extractEnumSymbols(JsonNode arrayNode) {
        Set<String> symbols = new LinkedHashSet<>();
        for (JsonNode element : arrayNode) {
            if (element.isTextual()) {
                symbols.add(element.asText());
            }
        }
        return new ArrayList<>(symbols);
    }

    /**
     * Infer record type from object, handling nested objects recursively.
     */
    private AvroTypeInfo inferRecordType(JsonNode objectNode, String fieldName) {
        Map<String, AvroTypeInfo> fields = new LinkedHashMap<>();

        objectNode.fieldNames().forEachRemaining(name -> {
            JsonNode fieldNode = objectNode.get(name);
            AvroTypeInfo fieldType = inferType(fieldNode, name);

            if (fieldNode.isNull()) {
                fieldType = createNullableType(Schema.Type.STRING);
            }

            fields.put(sanitizeFieldName(name), fieldType);
        });

        return AvroTypeInfo.builder()
                .avroType(Schema.Type.RECORD)
                .recordName(capitalize(fieldName))
                .fields(fields)
                .build();
    }

    /**
     * Create a nullable type (union of null and specified type).
     */
    private AvroTypeInfo createNullableType(Schema.Type type) {
        return AvroTypeInfo.builder()
                .avroType(Schema.Type.UNION)
                .addUnionType(AvroTypeInfo.builder().avroType(Schema.Type.NULL).build())
                .addUnionType(AvroTypeInfo.builder().avroType(type).build())
                .build();
    }

    /**
     * Make an existing type nullable.
     */
    private AvroTypeInfo makeNullable(AvroTypeInfo typeInfo) {
        return AvroTypeInfo.builder()
                .avroType(Schema.Type.UNION)
                .addUnionType(AvroTypeInfo.builder().avroType(Schema.Type.NULL).build())
                .addUnionType(typeInfo)
                .build();
    }

    /**
     * Check if the types list contains a similar type.
     */
    private boolean containsType(List<AvroTypeInfo> types, AvroTypeInfo newType) {
        for (AvroTypeInfo type : types) {
            if (type.getAvroType() == newType.getAvroType()) {
                if (type.getLogicalType() != null && type.getLogicalType().equals(newType.getLogicalType())) {
                    return true;
                }
                if (type.getLogicalType() == null && newType.getLogicalType() == null) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * Sanitize field name for Avro (replace spaces and special chars with
     * underscores).
     */
    private String sanitizeFieldName(String name) {
        return name.replaceAll("[^a-zA-Z0-9_]", "_");
    }

    /**
     * Capitalize first letter of a string.
     */
    private String capitalize(String str) {
        if (str == null || str.isEmpty()) {
            return str;
        }
        String sanitized = sanitizeFieldName(str);
        return Character.toUpperCase(sanitized.charAt(0)) + sanitized.substring(1);
    }
}
