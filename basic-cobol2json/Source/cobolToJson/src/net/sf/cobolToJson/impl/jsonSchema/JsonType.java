package net.sf.cobolToJson.impl.jsonSchema;

public enum JsonType {

	ARRAY("array", "items"),
	OBJECT("object", "properties"),
	STRING("string"),
	INTEGER("integer"),
	NUMBER("number")
	
	;
	final String typeName, nameOfChildren;
	final boolean hasChildren;

	private JsonType(String typeName) {
		this(typeName, null);
	}
	private JsonType(String typeName, String nameOfChildren) {
		this.typeName = typeName;
		this.nameOfChildren = nameOfChildren;
		this.hasChildren = nameOfChildren != null;
	}

	protected String getTypeName() {
		return typeName;
	}
}
