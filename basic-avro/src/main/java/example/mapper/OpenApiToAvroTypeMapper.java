package example.mapper;

import example.model.AvroTypeInfo;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.ArraySchema;
import io.swagger.v3.oas.models.media.Schema;
import org.apache.avro.Schema.Type;

import java.util.*;

public class OpenApiToAvroTypeMapper {

	private final OpenAPI openAPI;
	private final Map<String, String> processedSchemas;

	public OpenApiToAvroTypeMapper(OpenAPI openAPI) {
		this.openAPI = openAPI;
		this.processedSchemas = new HashMap<>();
	}

	public AvroTypeInfo mapSchema(Schema<?> schema, String fieldName) {
		if (schema == null) {
			return AvroTypeInfo.builder().avroType(Type.STRING).build();
		}

		if (schema.get$ref() != null) {
			return mapReference(schema.get$ref(), fieldName);
		}

		if (schema.getEnum() != null && !schema.getEnum().isEmpty()) {
			return mapEnum(schema, fieldName);
		}

		String type = schema.getType();
		String format = schema.getFormat();

		if (type == null) {
			return AvroTypeInfo.builder().avroType(Type.STRING).build();
		}

		switch (type.toLowerCase()) {
		case "string":
			return mapStringType(format, schema.getPattern());
		case "integer":
			return mapIntegerType(format);
		case "number":
			return mapNumberType(format);
		case "boolean":
			AvroTypeInfo.Builder boolBuilder = AvroTypeInfo.builder().avroType(Type.BOOLEAN);
			return boolBuilder.build();
		case "array":
			return mapArrayType((ArraySchema) schema, fieldName);
		case "object":
			return mapObjectType(schema, fieldName);
		default:
			return AvroTypeInfo.builder().avroType(Type.STRING).build();
		}
	}

	/**
	 * Map string type with format and pattern.
	 */
	private AvroTypeInfo mapStringType(String format, String pattern) {
		AvroTypeInfo.Builder builder = AvroTypeInfo.builder().avroType(Type.STRING);

		if (format != null) {
			switch (format.toLowerCase()) {
			case "uuid":
				builder.logicalType("uuid");
				break;
			case "date":
			case "date-time":
				builder.avroType(Type.LONG).logicalType("timestamp-millis");
				return builder.build();
			}
		}

		if (pattern != null && !pattern.isEmpty()) {
			builder.pattern(pattern);
		}

		return builder.build();
	}

	/**
	 * Map integer type with format.
	 */
	private AvroTypeInfo mapIntegerType(String format) {
		AvroTypeInfo.Builder builder;
		if ("int64".equals(format) || "long".equals(format)) {
			builder = AvroTypeInfo.builder().avroType(Type.LONG);
		} else {
			builder = AvroTypeInfo.builder().avroType(Type.INT);
		}

		return builder.build();
	}

	/**
	 * Map number type with format.
	 */
	private AvroTypeInfo mapNumberType(String format) {
		AvroTypeInfo.Builder builder;
		if ("double".equals(format)) {
			builder = AvroTypeInfo.builder().avroType(Type.DOUBLE);
		} else {
			builder = AvroTypeInfo.builder().avroType(Type.FLOAT);
		}

		return builder.build();
	}

	/**
	 * Map enum type.
	 */
	private AvroTypeInfo mapEnum(Schema<?> schema, String fieldName) {
		List<String> symbols = new ArrayList<>();
		for (Object enumValue : schema.getEnum()) {
			symbols.add(enumValue.toString());
		}

		AvroTypeInfo.Builder builder = AvroTypeInfo.builder().avroType(Type.ENUM).enumSymbols(symbols)
				.recordName(capitalize(fieldName));

		return builder.build();
	}

	/**
	 * Map array type.
	 */
	private AvroTypeInfo mapArrayType(ArraySchema arraySchema, String fieldName) {
		Schema<?> items = arraySchema.getItems();
		AvroTypeInfo itemType = mapSchema(items, fieldName);

		AvroTypeInfo.Builder builder = AvroTypeInfo.builder().avroType(Type.ARRAY).arrayItemType(itemType);

		return builder.build();
	}

	/**
	 * Map object type to record.
	 */
	private AvroTypeInfo mapObjectType(Schema<?> schema, String fieldName) {
		Map<String, AvroTypeInfo> fields = new LinkedHashMap<>();
		Map<String, Schema> properties = schema.getProperties();

		if (properties != null) {
			Set<String> requiredFields = schema.getRequired() != null ? new HashSet<>(schema.getRequired())
					: Collections.emptySet();

			for (Map.Entry<String, Schema> entry : properties.entrySet()) {
				String propName = entry.getKey();
				Schema<?> propSchema = entry.getValue();

				AvroTypeInfo fieldType = mapSchema(propSchema, propName);

				// Make field nullable if not required
				if (!requiredFields.contains(propName)) {
					fieldType = makeNullable(fieldType);
				}

				fields.put(sanitizeFieldName(propName), fieldType);
			}
		}

		AvroTypeInfo.Builder builder = AvroTypeInfo.builder().avroType(Type.RECORD).recordName(capitalize(fieldName))
				.fields(fields);

		return builder.build();
	}

	/**
	 * Map reference to another schema.
	 */
	private AvroTypeInfo mapReference(String ref, String fieldName) {
		String schemaName = extractSchemaNameFromRef(ref);

		if (openAPI.getComponents() != null && openAPI.getComponents().getSchemas() != null) {
			Schema<?> referencedSchema = openAPI.getComponents().getSchemas().get(schemaName);

			if (referencedSchema != null) {
				// Check if it's an enum
				if (referencedSchema.getEnum() != null && !referencedSchema.getEnum().isEmpty()) {
					return mapEnum(referencedSchema, schemaName);
				}

				// For objects, create a reference
				return mapSchema(referencedSchema, schemaName);
			}
		}

		return AvroTypeInfo.builder().avroType(Type.STRING).build();
	}

	/**
	 * Extract schema name from $ref.
	 */
	private String extractSchemaNameFromRef(String ref) {
		if (ref.contains("/")) {
			String[] parts = ref.split("/");
			return parts[parts.length - 1];
		}
		return ref;
	}

	/**
	 * Make a type nullable.
	 */
	private AvroTypeInfo makeNullable(AvroTypeInfo typeInfo) {
		AvroTypeInfo.Builder builder = AvroTypeInfo.builder().avroType(Type.UNION)
				.addUnionType(AvroTypeInfo.builder().avroType(Type.NULL).build()).addUnionType(typeInfo);

		return builder.build();
	}

	/**
	 * Sanitize field name for Avro.
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
