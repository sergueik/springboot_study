package example.dto.request;

import lombok.Data;

import javax.validation.constraints.NotBlank;

@Data
public class LoginRequest {

	public String getEmail() {
		return email;
	}

	public String getPassword() {
		return password;
	}

	@NotBlank
	private final String email;
	@NotBlank
	private final String password;
}
