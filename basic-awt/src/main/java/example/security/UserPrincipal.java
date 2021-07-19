package example.security;

import lombok.Data;

import org.springframework.security.core.AuthenticatedPrincipal;

import example.dto.UserDto;

@Data
public class UserPrincipal implements AuthenticatedPrincipal {

	private final UserDto userDto;

	@Override
	public String getName() {
		return this.userDto.getName();
	}
}
