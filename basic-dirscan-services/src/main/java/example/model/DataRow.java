package example.model;

public class DataRow {

	private String hostname;
	private String key;
	private String value;

	public String getHostname() {
		return hostname;
	}

	public void setHostname(String data) {
		hostname = data;
	}

	public DataRow withHostname(String data) {
		this.hostname = data;
		return this;
	}

	public DataRow withKey(String data) {
		this.key = data;
		return this;
	}

	public DataRow withValue(String data) {
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
