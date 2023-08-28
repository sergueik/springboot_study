package example.model;

import java.util.StringJoiner;

public class Person {

	private long id;
	private String name;
	private String address;

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getAddress() {
		return address;
	}

	public void setAddress(String address) {
		this.address = address;
	}

	@Override
	public String toString() {
		return new StringJoiner(", ", Person.class.getSimpleName() + "[", "]")
				.add("id=" + id).add("name='" + name + "'")
				.add("address='" + address + "'").toString();
	}
}
