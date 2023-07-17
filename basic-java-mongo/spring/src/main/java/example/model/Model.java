package example.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

import com.fasterxml.jackson.annotation.JsonInclude;

@JsonInclude(JsonInclude.Include.NON_NULL)
@Document
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
		value = data;
	}

	public Model(long id, String value) {
		this.id = id;
		this.value = value;
	}

	public Model() {
	}

}
