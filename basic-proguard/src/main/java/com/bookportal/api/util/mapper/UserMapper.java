package com.bookportal.api.util.mapper;

import com.bookportal.api.entity.User;
import com.bookportal.api.entity.softmodels.UserSoft;
import com.bookportal.api.model.SocialDTO;
import com.bookportal.api.model.UserRegisterDTO;
import reactor.core.publisher.Mono;

public class UserMapper {
    public static Mono<User> userRegisterDTOtoUser(UserRegisterDTO userRegisterDTO) {
        return Mono.fromSupplier(() -> {
            User user = new User();
            user.setMail(userRegisterDTO.getMail());
            user.setPassword(userRegisterDTO.getPassword());
            user.setName(userRegisterDTO.getName());
            user.setSurname(userRegisterDTO.getSurname());
            return user;
        });
    }

    public static UserSoft userToSoftUser(User user) {
        return UserSoft.builder()
                .id(user.getId())
                .name(user.getName())
                .surname(user.getSurname())
                .mail(user.getMail())
                .build();
    }
}
