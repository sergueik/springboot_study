package example.converter;

import com.fasterxml.jackson.databind.JsonNode;
import example.converter.interfaces.TypeDetector;
import example.model.AvroTypeInfo;
import org.apache.avro.Schema;

import java.util.*;
import java.util.stream.Collectors;

public class TypeInferenceEngine {

	private final List<TypeDetector> typeDetectors;

	public TypeInferenceEngine(List<TypeDetector> typeDetectors) {
		this.typeDetectors = typeDetectors != null ? new ArrayList<>(typeDetectors) : new ArrayList<>();
		this.typeDetectors.sort((d1, d2) -> Integer.compare(d2.getPriority(), d1.getPriority()));
	}

	public AvroTypeInfo inferType(JsonNode node, String fieldName) {
		if (node == null || node.isNull()) {
			return createNullableType(Schema.Type.STRING);
		}

		if (node.isBoolean()) {
			return AvroTypeInfo.builder().avroType(Schema.Type.BOOLEAN).build();
		}

		if (node.isNumber()) {
			// Traiter les nombres comme des strings
			return AvroTypeInfo.builder().avroType(Schema.Type.STRING).build();
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

		return AvroTypeInfo.builder().avroType(Schema.Type.STRING).build();
	}

	private AvroTypeInfo inferStringType(String value, String fieldName) {
		for (TypeDetector detector : typeDetectors) {
			if (detector.matches(value)) {
				if (detector.getLogicalType() != null) {
					return AvroTypeInfo.builder().avroType(Schema.Type.STRING).logicalType(detector.getLogicalType())
							.recordName(capitalize(fieldName)).build();
				}
			}
		}

		return AvroTypeInfo.builder().avroType(Schema.Type.STRING).build();
	}

	private AvroTypeInfo inferArrayType(JsonNode arrayNode, String fieldName) {
		if (arrayNode.isEmpty()) {
			return AvroTypeInfo.builder().avroType(Schema.Type.ARRAY)
					.arrayItemType(AvroTypeInfo.builder().avroType(Schema.Type.STRING).build()).build();
		}

		for (TypeDetector detector : typeDetectors) {
			if (detector.matchesArray(arrayNode)) {
				if ("uuid".equals(detector.getLogicalType())) {
					return AvroTypeInfo.builder().avroType(Schema.Type.ARRAY)
							.arrayItemType(
									AvroTypeInfo.builder().avroType(Schema.Type.STRING).logicalType("uuid").build())
							.build();
				} else {
					List<String> symbols = extractEnumSymbols(arrayNode);
					return AvroTypeInfo.builder().avroType(Schema.Type.ARRAY).arrayItemType(AvroTypeInfo.builder()
							.avroType(Schema.Type.ENUM).enumSymbols(symbols).recordName(capitalize(fieldName)).build())
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
			return AvroTypeInfo.builder().avroType(Schema.Type.ARRAY)
					.arrayItemType(createNullableType(Schema.Type.STRING)).build();
		}

		AvroTypeInfo itemType;
		if (types.size() == 1) {
			itemType = types.get(0);
			if (hasNull) {
				itemType = makeNullable(itemType);
			}
		} else {
			AvroTypeInfo.Builder unionBuilder = AvroTypeInfo.builder().avroType(Schema.Type.UNION);
			if (hasNull) {
				unionBuilder.addUnionType(AvroTypeInfo.builder().avroType(Schema.Type.NULL).build());
			}
			types.forEach(unionBuilder::addUnionType);
			itemType = unionBuilder.build();
		}

		return AvroTypeInfo.builder().avroType(Schema.Type.ARRAY).arrayItemType(itemType).build();
	}

	private List<String> extractEnumSymbols(JsonNode arrayNode) {
		Set<String> symbols = new LinkedHashSet<>();
		for (JsonNode element : arrayNode) {
			if (element.isTextual()) {
				symbols.add(element.asText());
			}
		}
		return new ArrayList<>(symbols);
	}

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

		return AvroTypeInfo.builder().avroType(Schema.Type.RECORD).recordName(capitalize(fieldName)).fields(fields)
				.build();
	}

	private AvroTypeInfo createNullableType(Schema.Type type) {
		return AvroTypeInfo.builder().avroType(Schema.Type.UNION)
				.addUnionType(AvroTypeInfo.builder().avroType(Schema.Type.NULL).build())
				.addUnionType(AvroTypeInfo.builder().avroType(type).build()).build();
	}

	private AvroTypeInfo makeNullable(AvroTypeInfo typeInfo) {
		return AvroTypeInfo.builder().avroType(Schema.Type.UNION)
				.addUnionType(AvroTypeInfo.builder().avroType(Schema.Type.NULL).build()).addUnionType(typeInfo).build();
	}

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

	private String sanitizeFieldName(String name) {
		return name.replaceAll("[^a-zA-Z0-9_]", "_");
	}

	private String capitalize(String str) {
		if (str == null || str.isEmpty()) {
			return str;
		}
		String sanitized = sanitizeFieldName(str);
		return Character.toUpperCase(sanitized.charAt(0)) + sanitized.substring(1);
	}
}
