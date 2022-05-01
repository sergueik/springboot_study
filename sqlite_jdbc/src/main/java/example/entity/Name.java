package example.entity;

import java.io.Serializable;
import java.sql.Timestamp;

public class Name implements Serializable {

	private static final long serialVersionUID = 6973576143316146252L;
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String value) {
		name = value;
	}

	@Override
	public String toString() {
		return "Student{" + "name=" + this.name + '}';
	}
}
