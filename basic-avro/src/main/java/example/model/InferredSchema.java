package example.model;

import java.util.LinkedHashMap;
import java.util.Map;

public class InferredSchema {

	private final String recordName;
	private final String namespace;
	private final Map<String, AvroTypeInfo> fields;

	public InferredSchema(String recordName, String namespace, Map<String, AvroTypeInfo> fields) {
		this.recordName = recordName;
		this.namespace = namespace;
		this.fields = fields != null ? fields : new LinkedHashMap<>();
	}

	public String getRecordName() {
		return recordName;
	}

	public String getNamespace() {
		return namespace;
	}

	public Map<String, AvroTypeInfo> getFields() {
		return fields;
	}

	@Override
	public String toString() {
		return "InferredSchema{" + "recordName='" + recordName + '\'' + ", namespace='" + namespace + '\''
				+ ", fieldsCount=" + fields.size() + '}';
	}
}
