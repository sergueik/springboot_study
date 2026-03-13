package example.serializer;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializationFeature;
import org.apache.avro.LogicalType;
import org.apache.avro.Schema;

import java.io.File;
import java.io.IOException;
import java.time.Instant;
import java.util.*;

public class AvroJsonGenerator {

	private final ObjectMapper objectMapper;

	public AvroJsonGenerator() {
		this.objectMapper = new ObjectMapper();
		this.objectMapper.enable(SerializationFeature.INDENT_OUTPUT);
	}

	public String generate(Schema schema) throws IOException {
		Object sample = generateValue(schema);
		return objectMapper.writeValueAsString(sample);
	}

	public void generateToFile(Schema schema, String outputPath) throws IOException {
		Object sample = generateValue(schema);
		objectMapper.writeValue(new File(outputPath), sample);
	}

	Object generateValue(Schema schema) {
		LogicalType logicalType = schema.getLogicalType();

		switch (schema.getType()) {
		case RECORD:
			return generateRecord(schema);
		case ENUM:
			return schema.getEnumSymbols().get(0);
		case ARRAY:
			return List.of(generateValue(schema.getElementType()));
		case MAP:
			return Collections.emptyMap();
		case UNION:
			return generateUnion(schema);
		case STRING:
			if (logicalType != null && "uuid".equals(logicalType.getName())) {
				return UUID.randomUUID().toString();
			}
			return "example_string";
		case BYTES:
			return "";
		case INT:
			return 0;
		case LONG:
			if (logicalType != null && "timestamp-millis".equals(logicalType.getName())) {
				return Instant.now().toEpochMilli();
			}
			return 0L;
		case FLOAT:
			return 0.0f;
		case DOUBLE:
			return 0.0;
		case BOOLEAN:
			return false;
		case NULL:
			return null;
		case FIXED:
			return new byte[schema.getFixedSize()];
		default:
			return null;
		}
	}

	private Map<String, Object> generateRecord(Schema schema) {
		Map<String, Object> record = new LinkedHashMap<>();
		for (Schema.Field field : schema.getFields()) {
			record.put(field.name(), generateValue(field.schema()));
		}
		return record;
	}

	private Object generateUnion(Schema schema) {
		List<Schema> types = schema.getTypes();

		// Find the non-null type in the union
		Schema nonNullType = types.stream().filter(t -> t.getType() != Schema.Type.NULL).findFirst().orElse(null);

		if (nonNullType == null) {
			return null;
		}

		// Generate the value for the non-null type
		Object value = generateValue(nonNullType);

		// Wrap in Avro JSON encoding format: {"typeName": value}
		String typeName = getAvroJsonTypeName(nonNullType);
		Map<String, Object> wrapped = new LinkedHashMap<>();
		wrapped.put(typeName, value);
		return wrapped;
	}

	private String getAvroJsonTypeName(Schema schema) {
		switch (schema.getType()) {
		case RECORD:
		case ENUM:
		case FIXED:
			return schema.getFullName();
		case ARRAY:
			return "array";
		case MAP:
			return "map";
		case STRING:
			return "string";
		case BYTES:
			return "bytes";
		case INT:
			return "int";
		case LONG:
			return "long";
		case FLOAT:
			return "float";
		case DOUBLE:
			return "double";
		case BOOLEAN:
			return "boolean";
		case NULL:
			return "null";
		default:
			return schema.getType().getName();
		}
	}
}
