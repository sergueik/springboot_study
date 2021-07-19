package org.example.ua.security;

import lombok.Data;
import org.example.ua.dto.UserDto;
import org.springframework.security.core.AuthenticatedPrincipal;

@Data
public class UserPrincipal implements AuthenticatedPrincipal {

    private final UserDto userDto;

    @Override
    public String getName() {
        return this.userDto.getName();
    }
}
