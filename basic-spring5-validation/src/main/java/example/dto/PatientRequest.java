package example.dto;

import example.model.Patient;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import javax.validation.constraints.AssertTrue;
import javax.validation.constraints.Email;
import javax.validation.constraints.FutureOrPresent;
import javax.validation.constraints.Max;
import javax.validation.constraints.Min;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Null;
import javax.validation.constraints.Past;
import javax.validation.constraints.Pattern;
import javax.validation.constraints.PositiveOrZero;
import javax.validation.constraints.Size;


@AllArgsConstructor
@NoArgsConstructor
@Builder
@Data
public class PatientRequest {

  @NotNull
	private String firstName;
  @NotEmpty
  private String middleName;
  @NotBlank
	private String lastName;
  
  @Min(value = 18, message = "The age should not be lower than {value}")
  @Max(value = 150, message = "The age should not be higher than {value}")
	private Integer age;

  //  @Past
  @Email
	private String email;

  // TODO: @Pattern(regexp = "(A|B|AB|O)[+-]")

	public String getFirstName() {
		return firstName;
	}

	public void setFirstName(String firstName) {
		this.firstName = firstName;
	}

	public String getMiddleName() {
		return middleName;
	}

	public void setMiddleName(String middleName) {
		this.middleName = middleName;
	}

	public String getLastName() {
		return lastName;
	}

	public void setLastName(String lastName) {
		this.lastName = lastName;
	}

	public Integer getAge() {
		return age;
	}

	public void setAge(Integer age) {
		this.age = age;
	}

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public Patient toEntity() {
		return Patient.builder().firstName(firstName).middleName(middleName)
				.lastName(lastName).age(age).email(email).build();
	}
}
