package example.model;

public class Employee {

	public Employee() {

	}

	public Employee(Integer id, String firstName, String lastName, String email) {
		super();
		this.id = id;
		this.firstName = firstName;
		this.lastName = lastName;
		this.email = email;
	}

	private Integer id;
	private String firstName;
	private String lastName;
	private String email;

	public Integer getId() {
		return id;
	}

	public void setId(Integer data) {
		id = data;
	}

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String data) {
		firstName = data;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String data) {
		lastName = data;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String data) {
		email = data;
	}

	@Override
	public String toString() {
		return "Employee [id=" + id + ", firstName=" + firstName + ", lastName="
				+ lastName + ", email=" + email + "]";
	}
}
