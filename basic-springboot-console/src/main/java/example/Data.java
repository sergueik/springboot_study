package example;

import org.springframework.jdbc.core.RowMapper;

public class Data {
	private final long id;
	private final String key;
	private final String value;

	public Data(long id, String key, String value) {
		this.id = id;
		this.key = key;
		this.value = value;
	}

	public static RowMapper<Data> getRowMapper() {
		return (rs, rowNum) -> new Data(rs.getLong("id"), rs.getString("key"),
				rs.getString("value"));
	}

	@Override
	public String toString() {
		return "app.Data{" + "id=" + id + ", key='" + key + "'" + ", value=" + value
				+ "}";
	}

	public String getKey() {
		return key;
	}

	public String getValue() {
		return value;
	}

	public long getId() {
		return id;
	}
}
