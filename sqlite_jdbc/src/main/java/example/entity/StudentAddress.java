package example.entity;

public class StudentAddress {
	private Student student;
	private Address address;

	public long getId() {
		return student.getId();
	}

	public void setId(long value) {
		student.setId(value);
	}

	public String getName() {
		return student.getName();
	}

	public void setName(String value) {
		student.setName(value);
	}

	public String getCourse() {
		return student.getCourse();
	}

	public void setCourse(String value) {
		student.setCourse(value);
	}

	public long getAddressId() {
		return address.getAddressId();
	}

	public void setAddressId(long value) {
		address.setAddressId(value);
	}

	public String getStreet() {
		return address.getStreet();
	}

	public void setStreet(String value) {
		address.setStreet(value);
	}

	public String getCity() {
		return address.getCity();
	}

	public void setCity(String value) {
		address.setCity(value);
	}

	public String getState() {
		return address.getState();
	}

	public void setState(String value) {
		address.setState(value);
	}

	public String getZipcode() {
		return address.getZipcode();
	}

	public void setZipcode(String value) {
		address.setZipcode(value);
	}

	public StudentAddress() {
		this.address = new Address();
		this.student = new Student();
	}

}
