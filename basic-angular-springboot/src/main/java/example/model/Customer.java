package example.model;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Customer {

	@JsonProperty(value = "firstname")
	private String firstName;

	@JsonProperty(value = "lastname")
	private String lastName;

	@JsonProperty(value = "dateofbirth")
	private String birthDate;

	@JsonProperty(value = "ssn")
	private String ssn;

	private int score;

	public Customer() {
	}

	public Customer(String firstName, String lastName, String birthDate, String ssn) {
		this.firstName = firstName;
		this.lastName = lastName;
		this.birthDate = birthDate;
		this.ssn = ssn;
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

	public String getBirthDate() {
		return birthDate;
	}

	public void setBirthDate(String data) {
		birthDate = data;
	}

	public String getSsn() {
		return ssn;
	}

	public void setSsn(String data) {
		ssn = data;
	}

	public int getScore() {
		return score;
	}

	public void setScore(int data) {
		score = data;
	}

	@Override
	public String toString() {
		return "Customer {" + "First name='" + this.firstName + '\'' + ", Last name=" + this.lastName
				+ ", Date of birth=" + this.birthDate + ", SSN=" + this.ssn + '}';
	}

}
