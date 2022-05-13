package example.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "address")
public class Address {

	private long addressId;
	private String street;
	private String city;
	private String state;
	private String zipcode;

	public Address() {
	}

	public Address(String street, String city, String state, String zipcode) {
		this.street = street;
		this.city = city;
		this.state = state;
		this.zipcode = zipcode;
	}

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	@Column(name = "aid")
	public long getAddressId() {
		return addressId;
	}

	public void setAddressId(long value) {
		addressId = value;
	}

	@Column(name = "astreet", nullable = false, length = 250)
	public String getStreet() {
		return street;
	}

	public void setStreet(String value) {
		street = value;
	}

	@Column(name = "acity", nullable = false, length = 50)
	public String getCity() {
		return city;
	}

	public void setCity(String value) {
		city = value;
	}

	@Column(name = "astate", nullable = false, length = 50)
	public String getState() {
		return state;
	}

	public void setState(String value) {
		state = value;
	}

	@Column(name = "azipcode", nullable = false, length = 10)
	public String getZipcode() {
		return zipcode;
	}

	public void setZipcode(String value) {
		zipcode = value;
	}
}
