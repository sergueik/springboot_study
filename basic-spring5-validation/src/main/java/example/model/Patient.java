package example.model;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.persistence.*;

import example.dto.PatientRequest;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
@Table(name = "patient")
@Entity
public class Patient {

	@Id
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Long id;

	@Column(name = "first_name")
	private String firstName;

	@Column(name = "middle_name")
	private String middleName;

	@Column(name = "last_name")
	private String lastName;

	@Column(name = "age")
	private Integer age;

	@Column(name = "email")
	private String email;

	public Patient updatePatient(PatientRequest patientRequest) {
		setFirstName(patientRequest.getFirstName());
		setMiddleName(patientRequest.getMiddleName());
		setLastName(patientRequest.getLastName());
		setAge(patientRequest.getAge());
		setEmail(patientRequest.getEmail());
		// setBloodType(patientRequest.getBloodType());
		// setConsentGiven(patientRequest.getConsentGiven());
		// setPreexistingConditions(patientRequest.getPreexistingConditions());
		// setPolicyNumber(patientRequest.getPolicyNumber());
		// setRegistrationDate(patientRequest.getRegistrationDate());
		// setDateOfBirth(patientRequest.getDateOfBirth());
		return this;
	}

}
