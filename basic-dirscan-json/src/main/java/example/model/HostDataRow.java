package example.model;

public class HostDataRow {

	private String key;
	private String value;

	public HostDataRow withKey(String data) {
		this.key = data;
		return this;
	}

	public HostDataRow withValue(String data) {
		this.value = data;
		return this;
	}

	public String getKey() {
		return key;
	}

	public void setKey(String data) {
		key = data;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String data) {
		value = data;
	}

}
