package org.example.ua.dto;

import lombok.Data;
import org.example.ua.entity.User;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotEmpty;
import java.util.List;

@Data
public class UserDto {

    @NotBlank
    private final String name;
    @NotBlank
    private final String email;
    @NotEmpty
    private final List<User.Role> roles;

    public static UserDto from(User user) {
        return new UserDto(
                user.getName(),
                user.getEmail(),
                user.getRolesList()
        );
    }

    public static User to(UserDto userDto) {
        User user = new User();
        user.setName(userDto.getName());
        user.setEmail(userDto.getEmail());
        user.setRolesList(userDto.getRoles().toArray(new User.Role[0]));
        return user;
    }
}
