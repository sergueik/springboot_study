package example.model;

import org.apache.avro.Schema;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

public class AvroTypeInfo {

	private final Schema.Type avroType;
	private final boolean nullable;
	private final List<AvroTypeInfo> unionTypes;
	private final String logicalType;
	private final Map<String, AvroTypeInfo> fields;
	private final List<String> enumSymbols;
	private final AvroTypeInfo arrayItemType;
	private final String recordName;
	private final String pattern;

	private AvroTypeInfo(Builder builder) {
		this.avroType = builder.avroType;
		this.nullable = builder.nullable;
		this.unionTypes = builder.unionTypes;
		this.logicalType = builder.logicalType;
		this.fields = builder.fields;
		this.enumSymbols = builder.enumSymbols;
		this.arrayItemType = builder.arrayItemType;
		this.recordName = builder.recordName;
		this.pattern = builder.pattern;
	}

	public Schema.Type getAvroType() {
		return avroType;
	}

	public boolean isNullable() {
		return nullable;
	}

	public List<AvroTypeInfo> getUnionTypes() {
		return unionTypes;
	}

	public String getLogicalType() {
		return logicalType;
	}

	public Map<String, AvroTypeInfo> getFields() {
		return fields;
	}

	public List<String> getEnumSymbols() {
		return enumSymbols;
	}

	public AvroTypeInfo getArrayItemType() {
		return arrayItemType;
	}

	public String getRecordName() {
		return recordName;
	}

	public String getPattern() {
		return pattern;
	}

	public static class Builder {
		private Schema.Type avroType;
		private boolean nullable = false;
		private List<AvroTypeInfo> unionTypes = new ArrayList<>();
		private String logicalType;
		private Map<String, AvroTypeInfo> fields = new LinkedHashMap<>();
		private List<String> enumSymbols = new ArrayList<>();
		private AvroTypeInfo arrayItemType;
		private String recordName;
		private String pattern;

		public Builder avroType(Schema.Type avroType) {
			this.avroType = avroType;
			return this;
		}

		public Builder nullable(boolean nullable) {
			this.nullable = nullable;
			return this;
		}

		public Builder unionTypes(List<AvroTypeInfo> unionTypes) {
			this.unionTypes = unionTypes;
			return this;
		}

		public Builder addUnionType(AvroTypeInfo typeInfo) {
			this.unionTypes.add(typeInfo);
			return this;
		}

		public Builder logicalType(String logicalType) {
			this.logicalType = logicalType;
			return this;
		}

		public Builder fields(Map<String, AvroTypeInfo> fields) {
			this.fields = fields;
			return this;
		}

		public Builder addField(String name, AvroTypeInfo typeInfo) {
			this.fields.put(name, typeInfo);
			return this;
		}

		public Builder enumSymbols(List<String> symbols) {
			this.enumSymbols = symbols;
			return this;
		}

		public Builder addEnumSymbol(String symbol) {
			if (!this.enumSymbols.contains(symbol)) {
				this.enumSymbols.add(symbol);
			}
			return this;
		}

		public Builder arrayItemType(AvroTypeInfo itemType) {
			this.arrayItemType = itemType;
			return this;
		}

		public Builder recordName(String recordName) {
			this.recordName = recordName;
			return this;
		}

		public Builder pattern(String pattern) {
			this.pattern = pattern;
			return this;
		}

		public AvroTypeInfo build() {
			Objects.requireNonNull(avroType, "avroType must not be null");
			return new AvroTypeInfo(this);
		}
	}

	public static Builder builder() {
		return new Builder();
	}

	@Override
	public String toString() {
		return "AvroTypeInfo{" + "avroType=" + avroType + ", nullable=" + nullable + ", logicalType='" + logicalType
				+ '\'' + ", recordName='" + recordName + '\'' + '}';
	}
}
