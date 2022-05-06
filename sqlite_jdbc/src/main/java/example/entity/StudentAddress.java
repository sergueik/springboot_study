package example.entity;

public class StudentAddress {

	private long addressId;
	private String street;
	private String city;
	private String state;
	private String zipcode;
	private long id;
	private String name;
	private String course;

	// NOTE: these fields are set via reflection
	// dispatch the setters and getters wil lnot work

	public long getId() {
		// return student.getId();
		return id;
	}

	public void setId(long value) {
		// student.setId(value);
		id = value;
	}

	public String getName() {
		// return student.getName();
		return name;
	}

	public void setName(String value) {
		// student.setName(value);
		name = value;
	}

	public String getCourse() {
		// return student.getCourse();
		return course;
	}

	public void setCourse(String value) {
		// student.setCourse(value);
		course = value;
	}

	public long getAddressId() {
		// return address.getAddressId();
		return this.addressId;
	}

	public void setAddressId(long addressId) {
		// address.setAddressId(value);
		this.addressId = addressId;
	}

	public String getStreet() {
		// return address.getStreet();
		return this.street;
	}

	public void setStreet(String street) {
		// address.setStreet(value);
		this.street = street;
	}

	public String getCity() {
		// return address.getCity();
		return this.city;
	}

	public void setCity(String city) {
		// address.setCity(value);
		this.city = city;
	}

	public String getState() {
		// return address.getState();
		return this.state;
	}

	public void setState(String state) {
		// address.setState(value);
		this.state = state;
	}

	public String getZipcode() {
		// return address.getZipcode();
		return this.zipcode;
	}

	public void setZipcode(String zipcode) {
		// address.setZipcode(value);
		this.zipcode = zipcode;
	}

	public StudentAddress() {
	}

}
