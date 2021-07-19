package example.dto.request;

import com.fasterxml.jackson.annotation.JsonIgnore;

import example.validation.ValidPassword;
import lombok.Data;

import javax.validation.constraints.AssertTrue;
import javax.validation.constraints.NotBlank;

@Data
public class RegistrationRequest {

    @NotBlank
    private final String name;
    @NotBlank
    private final String email;
    @ValidPassword
    private final String password;
    @NotBlank
    private final String password2;

    @AssertTrue(message = "Passwords do not match")
    @JsonIgnore
    public boolean isPassword2Correct() {
        return password != null
                && password2 != null
                && password.equals(password2);
    }
}
