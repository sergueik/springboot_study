package example.domain;

public class Employee implements Comparable<Employee> {

	private int id;
	private String name;
	private String location;

	public Employee() {
	}

	public String toString() {
		return String.format("id: %d, name: \"%s\"", id, name);
	}

	public Employee(int id, String name, String location) {
		this.id = id;
		this.name = name;
		this.location = location;
	}

	public int getId() {
		return id;
	}

	public void setId(int value) {
		id = value;
	}

	public String getName() {
		return name;
	}

	@Override
	public int compareTo(Employee o) {
		return 0;
	}
}