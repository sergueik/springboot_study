package example.converter;

import example.model.AvroTypeInfo;
import org.apache.avro.LogicalTypes;
import org.apache.avro.Schema;

import java.util.*;

/**
 * Generates Avro schemas from inferred type information.
 *
 * This class follows the Single Responsibility Principle by focusing solely on
 * schema generation from AvroTypeInfo.
 */
public class SchemaGenerator {

    private static final String DEFAULT_NAMESPACE = "example.generated";
    private final Map<String, Schema> enumSchemaCache = new HashMap<>();
    private int enumCounter = 0;

    /**
     * Generate an Avro schema from the root type information.
     *
     * @param rootType   the root type information
     * @param recordName the name for the root record
     * @return the generated Avro schema
     */
    public Schema generateSchema(AvroTypeInfo rootType, String recordName) {
        enumSchemaCache.clear();
        enumCounter = 0;

        if (rootType.getAvroType() == Schema.Type.RECORD) {
            return generateRecordSchema(rootType, recordName, DEFAULT_NAMESPACE);
        }

        return generateTypeSchema(rootType, recordName, DEFAULT_NAMESPACE);
    }

    /**
     * Generate a schema for any type.
     */
    private Schema generateTypeSchema(AvroTypeInfo typeInfo, String name, String namespace) {
        switch (typeInfo.getAvroType()) {
            case NULL:
                return Schema.create(Schema.Type.NULL);

            case BOOLEAN:
                return Schema.create(Schema.Type.BOOLEAN);

            case INT:
                return Schema.create(Schema.Type.INT);

            case LONG:
                return Schema.create(Schema.Type.LONG);

            case FLOAT:
                return Schema.create(Schema.Type.FLOAT);

            case DOUBLE:
                return Schema.create(Schema.Type.DOUBLE);

            case STRING:
                return generateStringSchema(typeInfo);

            case ARRAY:
                return generateArraySchema(typeInfo, name, namespace);

            case ENUM:
                return generateEnumSchema(typeInfo, name, namespace);

            case RECORD:
                return generateRecordSchema(typeInfo, name, namespace);

            case UNION:
                return generateUnionSchema(typeInfo, name, namespace);

            default:
                return Schema.create(Schema.Type.STRING);
        }
    }

    /**
     * Generate a string schema with optional logical type and pattern.
     * If the type has a name (recordName), create a named type.
     */
    private Schema generateStringSchema(AvroTypeInfo typeInfo) {
        if (typeInfo.getRecordName() != null && typeInfo.getLogicalType() != null) {
            // Créer un schéma avec un nom pour les types logiques
            StringBuilder schemaJson = new StringBuilder();
            schemaJson.append("{\"name\":\"").append(typeInfo.getRecordName()).append("\"");
            schemaJson.append(",\"type\":\"string\"");
            schemaJson.append(",\"logicalType\":\"").append(typeInfo.getLogicalType()).append("\"");
            if (typeInfo.getPattern() != null && !typeInfo.getPattern().isEmpty()) {
                // Escape backslashes and quotes in pattern
                String escapedPattern = typeInfo.getPattern()
                        .replace("\\", "\\\\")
                        .replace("\"", "\\\"");
                schemaJson.append(",\"pattern\":\"").append(escapedPattern).append("\"");
            }
            schemaJson.append("}");
            return new Schema.Parser().parse(schemaJson.toString());
        }

        // For string with pattern but no logical type
        if (typeInfo.getPattern() != null && !typeInfo.getPattern().isEmpty()) {
            StringBuilder schemaJson = new StringBuilder();
            schemaJson.append("{\"type\":\"string\"");
            String escapedPattern = typeInfo.getPattern()
                    .replace("\\", "\\\\")
                    .replace("\"", "\\\"");
            schemaJson.append(",\"pattern\":\"").append(escapedPattern).append("\"");
            if (typeInfo.getLogicalType() != null) {
                schemaJson.append(",\"logicalType\":\"").append(typeInfo.getLogicalType()).append("\"");
            }
            schemaJson.append("}");
            return new Schema.Parser().parse(schemaJson.toString());
        }

        Schema stringSchema = Schema.create(Schema.Type.STRING);

        if ("uuid".equals(typeInfo.getLogicalType())) {
            LogicalTypes.uuid().addToSchema(stringSchema);
        }

        return stringSchema;
    }

    /**
     * Generate an array schema.
     */
    private Schema generateArraySchema(AvroTypeInfo typeInfo, String name, String namespace) {
        AvroTypeInfo itemType = typeInfo.getArrayItemType();
        if (itemType == null) {
            itemType = AvroTypeInfo.builder()
                    .avroType(Schema.Type.STRING)
                    .build();
        }

        Schema itemSchema = generateTypeSchema(itemType, name, namespace);
        return Schema.createArray(itemSchema);
    }

    /**
     * Generate an enum schema.
     */
    private Schema generateEnumSchema(AvroTypeInfo typeInfo, String name, String namespace) {
        List<String> symbols = typeInfo.getEnumSymbols();
        if (symbols == null || symbols.isEmpty()) {
            symbols = Collections.singletonList("UNKNOWN");
        }

        String enumName = typeInfo.getRecordName() != null ? typeInfo.getRecordName() : sanitizeName(name);

        String cacheKey = namespace + "." + enumName;
        if (enumSchemaCache.containsKey(cacheKey)) {
            return enumSchemaCache.get(cacheKey);
        }

        Schema enumSchema = Schema.createEnum(enumName, null, namespace, symbols);
        enumSchemaCache.put(cacheKey, enumSchema);

        return enumSchema;
    }

    /**
     * Generate a record schema.
     */
    private Schema generateRecordSchema(AvroTypeInfo typeInfo, String name, String namespace) {
        String recordName = typeInfo.getRecordName() != null ? typeInfo.getRecordName() : sanitizeName(name);

        List<Schema.Field> fields = new ArrayList<>();

        if (typeInfo.getFields() != null) {
            for (Map.Entry<String, AvroTypeInfo> entry : typeInfo.getFields().entrySet()) {
                String fieldName = entry.getKey();
                AvroTypeInfo fieldType = entry.getValue();

                Schema fieldSchema = generateTypeSchema(fieldType, fieldName, namespace);

                // Ajouter default: null pour les champs nullable (union avec null en premier)
                Object defaultValue = null;
                if (fieldType.getAvroType() == Schema.Type.UNION &&
                        fieldType.getUnionTypes() != null &&
                        !fieldType.getUnionTypes().isEmpty() &&
                        fieldType.getUnionTypes().get(0).getAvroType() == Schema.Type.NULL) {
                    defaultValue = Schema.Field.NULL_VALUE;
                }

                Schema.Field field = new Schema.Field(fieldName, fieldSchema, null, defaultValue);
                fields.add(field);
            }
        }

        return Schema.createRecord(recordName, null, namespace, false, fields);
    }

    /**
     * Generate a union schema.
     */
    private Schema generateUnionSchema(AvroTypeInfo typeInfo, String name, String namespace) {
        List<Schema> unionTypes = new ArrayList<>();

        if (typeInfo.getUnionTypes() != null && !typeInfo.getUnionTypes().isEmpty()) {
            for (AvroTypeInfo unionType : typeInfo.getUnionTypes()) {
                Schema schema = generateTypeSchema(unionType, name, namespace);
                unionTypes.add(schema);
            }
        }

        return Schema.createUnion(unionTypes);
    }

    /**
     * Sanitize name for Avro schema (remove invalid characters).
     */
    private String sanitizeName(String name) {
        if (name == null || name.isEmpty()) {
            return "Field" + (enumCounter++);
        }

        String sanitized = name.replaceAll("[^a-zA-Z0-9_]", "_");

        if (!Character.isLetter(sanitized.charAt(0)) && sanitized.charAt(0) != '_') {
            sanitized = "_" + sanitized;
        }

        return sanitized;
    }

    /**
     * Convert the schema to pretty-printed JSON with custom formatting for named
     * types.
     *
     * @param schema the Avro schema
     * @return the pretty-printed JSON string
     */
    public String toPrettyJson(Schema schema) {
        String json = schema.toString(true);
        return json;
    }

    /**
     * Generate schema with custom type information preserved.
     */
    public String generateSchemaJson(AvroTypeInfo rootType, String recordName) {
        Schema schema = generateSchema(rootType, recordName);
        return buildCustomJson(schema, rootType, 0);
    }

    /**
     * Build custom JSON preserving type names from AvroTypeInfo.
     */
    private String buildCustomJson(Schema schema, AvroTypeInfo typeInfo, int indent) {
        StringBuilder json = new StringBuilder();
        String indentStr = "  ".repeat(indent);
        String indentStr1 = "  ".repeat(indent + 1);

        if (schema.getType() == Schema.Type.RECORD) {
            json.append("{\n");
            json.append(indentStr1).append("\"type\" : \"record\",\n");
            json.append(indentStr1).append("\"name\" : \"").append(schema.getName()).append("\",\n");
            json.append(indentStr1).append("\"namespace\" : \"").append(schema.getNamespace()).append("\",\n");
            json.append(indentStr1).append("\"fields\" : [ ");

            java.util.List<Schema.Field> fields = schema.getFields();
            for (int i = 0; i < fields.size(); i++) {
                Schema.Field field = fields.get(i);
                AvroTypeInfo fieldTypeInfo = typeInfo.getFields() != null ? typeInfo.getFields().get(field.name())
                        : null;

                if (i > 0)
                    json.append(", ");
                json.append("{\n");
                json.append(indentStr1).append("  \"name\" : \"").append(field.name()).append("\",\n");
                json.append(indentStr1).append("  \"type\" : ");

                if (fieldTypeInfo != null && fieldTypeInfo.getRecordName() != null &&
                        fieldTypeInfo.getLogicalType() != null) {
                    // Type avec nom personnalisé (UUID, etc.)
                    json.append("{\n");
                    json.append(indentStr1).append("    \"name\" : \"").append(fieldTypeInfo.getRecordName())
                            .append("\",\n");
                    json.append(indentStr1).append("    \"type\" : \"string\",\n");
                    json.append(indentStr1).append("    \"logicalType\" : \"").append(fieldTypeInfo.getLogicalType())
                            .append("\"");
                    if (fieldTypeInfo.getPattern() != null && !fieldTypeInfo.getPattern().isEmpty()) {
                        json.append(",\n");
                        json.append(indentStr1).append("    \"pattern\" : \"").append(fieldTypeInfo.getPattern())
                                .append("\"");
                    }
                    json.append("\n");
                    json.append(indentStr1).append("  }");
                } else {
                    json.append(field.schema().toString(true).replace("\n", "\n" + indentStr1 + "  "));
                }

                // Ajouter default: null pour les champs nullable
                if (field.defaultVal() != null && field.defaultVal().equals(Schema.Field.NULL_VALUE)) {
                    json.append(",\n");
                    json.append(indentStr1).append("  \"default\" : null");
                }

                json.append("\n").append(indentStr1).append("}");
            }

            json.append(" ]\n");
            json.append(indentStr).append("}");
        } else {
            return schema.toString(true);
        }

        return json.toString();
    }
}
