package example;

import org.springframework.data.annotation.Id;

import com.fasterxml.jackson.annotation.JsonInclude;

@JsonInclude(JsonInclude.Include.NON_NULL)
public class Model {

	@Id
	private long id;

	private String value;

	public long getId() {
		return id;
	}

	public void setId(long data) {
		id = data;
	}

	public String getValue() {
		return value;
	}

	public void setValue(String data) {
		value = value;
	}

	public Model(long id, String value) {
		this.id = id;
		this.value = value;
	}

	public Model() {
	}

}
