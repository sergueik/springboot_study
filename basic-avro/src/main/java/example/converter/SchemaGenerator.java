package example.converter;

import example.model.AvroTypeInfo;
import org.apache.avro.LogicalTypes;
import org.apache.avro.Schema;

import java.util.*;

public class SchemaGenerator {

	private static final String DEFAULT_NAMESPACE = "example.generated";
	private final Map<String, Schema> enumSchemaCache = new HashMap<>();
	private int enumCounter = 0;

	public Schema generateSchema(AvroTypeInfo rootType, String recordName) {
		enumSchemaCache.clear();
		enumCounter = 0;

		if (rootType.getAvroType() == Schema.Type.RECORD) {
			return generateRecordSchema(rootType, recordName, DEFAULT_NAMESPACE);
		}

		return generateTypeSchema(rootType, recordName, DEFAULT_NAMESPACE);
	}

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

	private Schema generateStringSchema(AvroTypeInfo typeInfo) {
		if (typeInfo.getRecordName() != null && typeInfo.getLogicalType() != null) {
			StringBuilder schemaJson = new StringBuilder();
			schemaJson.append("{\"name\":\"").append(typeInfo.getRecordName()).append("\"");
			schemaJson.append(",\"type\":\"string\"");
			schemaJson.append(",\"logicalType\":\"").append(typeInfo.getLogicalType()).append("\"");
			if (typeInfo.getPattern() != null && !typeInfo.getPattern().isEmpty()) {
				String escapedPattern = typeInfo.getPattern().replace("\\", "\\\\").replace("\"", "\\\"");
				schemaJson.append(",\"pattern\":\"").append(escapedPattern).append("\"");
			}
			schemaJson.append("}");
			return new Schema.Parser().parse(schemaJson.toString());
		}

		if (typeInfo.getPattern() != null && !typeInfo.getPattern().isEmpty()) {
			StringBuilder schemaJson = new StringBuilder();
			schemaJson.append("{\"type\":\"string\"");
			String escapedPattern = typeInfo.getPattern().replace("\\", "\\\\").replace("\"", "\\\"");
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

	private Schema generateArraySchema(AvroTypeInfo typeInfo, String name, String namespace) {
		AvroTypeInfo itemType = typeInfo.getArrayItemType();
		if (itemType == null) {
			itemType = AvroTypeInfo.builder().avroType(Schema.Type.STRING).build();
		}

		Schema itemSchema = generateTypeSchema(itemType, name, namespace);
		return Schema.createArray(itemSchema);
	}

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
				if (fieldType.getAvroType() == Schema.Type.UNION && fieldType.getUnionTypes() != null
						&& !fieldType.getUnionTypes().isEmpty()
						&& fieldType.getUnionTypes().get(0).getAvroType() == Schema.Type.NULL) {
					defaultValue = Schema.Field.NULL_VALUE;
				}

				Schema.Field field = new Schema.Field(fieldName, fieldSchema, null, defaultValue);
				fields.add(field);
			}
		}

		return Schema.createRecord(recordName, null, namespace, false, fields);
	}

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

	public String toPrettyJson(Schema schema) {
		String json = schema.toString(true);
		return json;
	}

	public String generateSchemaJson(AvroTypeInfo rootType, String recordName) {
		Schema schema = generateSchema(rootType, recordName);
		return buildCustomJson(schema, rootType, 0);
	}

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

				if (fieldTypeInfo != null && fieldTypeInfo.getRecordName() != null
						&& fieldTypeInfo.getLogicalType() != null) {
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
