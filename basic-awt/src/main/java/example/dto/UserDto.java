package example.dto;

import lombok.Data;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;

import example.entity.User;

import java.util.List;

@Data
public class UserDto {

	@NotBlank
	private final String name;
	@NotBlank
	private final String email;

	public String getName() {
		return name;
	}

	public String getEmail() {
		return email;
	}

	public List<User.Role> getRolesList() {
		return roles;
	}

	@NotEmpty
	private final List<User.Role> roles;

	public static UserDto from(User user) {
		return new UserDto(user.getName(), user.getEmail(), user.getRolesList());
	}

	public static User to(UserDto userDto) {
		User user = new User();
		user.setName(userDto.getName());
		user.setEmail(userDto.getEmail());
		user.setRolesList(userDto.getRoles().toArray(new User.Role[0]));
		return user;
	}
}
