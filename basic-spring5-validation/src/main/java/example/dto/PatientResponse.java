package example.dto;

import example.model.Patient;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class PatientResponse {

	private Long id;
	private String firstName;
	private String middleName;
	private String lastName;
	private Integer age;
	private String email;

	public static PatientResponse fromEntity(Patient patient) {
		return PatientResponse.builder().id(patient.getId())
				.firstName(patient.getFirstName()).middleName(patient.getMiddleName())
				.lastName(patient.getLastName()).age(patient.getAge())
				.email(patient.getEmail()).build();
	}
}
