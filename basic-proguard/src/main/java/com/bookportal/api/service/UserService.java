package com.bookportal.api.service;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.entity.BaseEntity;
import com.bookportal.api.entity.Role;
import com.bookportal.api.entity.User;
import com.bookportal.api.exception.CustomAlreadyExistException;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.PasswordUpdateDTO;
import com.bookportal.api.model.SocialDTO;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.model.enums.UserRoleEnum;
import com.bookportal.api.repository.UserRepository;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.dao.DuplicateKeyException;
import org.springframework.http.HttpStatus;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.ReactiveSecurityContextHolder;
import org.springframework.security.core.context.SecurityContext;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Objects;

@Slf4j
@RequiredArgsConstructor
@Service
public class UserService {
    private final UserRepository userRepository;
    private final EnvironmentVariables env;
    private final EmailService emailService;

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Boolean> updatePassword(String mail, String newPassword) {
        return userRepository.findByMailAndActiveTrue(mail)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.USER.getValue())))
                .map(user -> {
                    user.setPassword(new BCryptPasswordEncoder().encode(newPassword));
                    return user;
                })
                .flatMap(userRepository::save)
                .map(BaseEntity::isActive);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<User> createUser(Mono<User> userMono) {
        return userMono
                .onErrorStop()
                .doOnNext(user -> isMailExist(user.getMail()))
                .map(user -> {
                    user.setActive(false);
                    user.setPassword(new BCryptPasswordEncoder().encode(user.getPassword()));
                    user.setSocial(false);
                    user.setRoles(new ArrayList<>(Collections.singletonList(Role.builder().name(UserRoleEnum.ROLE_USER.name()).build())));
                    return user;
                })
                .flatMap(userRepository::save)
                .doOnError(throwable -> {
                    if (throwable instanceof DuplicateKeyException) {
                        String[] cause = throwable.getMessage().split("\\{")[1].split("}")[0].split(":");
                        String key = cause[0].substring(0, 1).toUpperCase() + cause[0].substring(1);
                        String value = cause[1].replace("\"", "");
                        throw new CustomAlreadyExistException("User {" + key + ":" + value + "} already exists");
                    }
                })
                .doOnNext(emailService::sendEmailConfirmationLink);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<User> setUserToActive(String id) {
        return userRepository.findById(id)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.USER.getValue())))
                .flatMap(user -> {
                    user.setActive(true);
                    return userRepository.save(user);
                });
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<User> saveGuest(Mono<User> userMono) {
        Mono<User> notFoundUserInit = userMono
                .map(user -> {
                    user.setRoles(new ArrayList<>(Collections.singletonList(Role.builder()
                            .name(UserRoleEnum.ROLE_GUEST.name())
                            .build())));
                    return user;
                });

        return userMono.flatMap(user -> userRepository.findByMail(user.getMail()))
                .switchIfEmpty(notFoundUserInit)
                .flatMap(userRepository::save);
    }

    private Mono<User> isMailExist(String mail) {
        return userRepository.findByMail(mail)
                .doOnNext(user1 -> Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.eMailAlreadyInUse())));
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<User> createSocialUser(SocialDTO socialDto) {
        return userRepository.findByMail(socialDto.getMail())
                .doOnNext(user -> {
                    if (Boolean.FALSE.equals(user.getSocial())) {
                        user.setGoogleId(socialDto.getGoogleId());
                        user.setFacebookId(socialDto.getFacebookId());
                    }
                    // update socialId's if same email
                    if (Objects.equals(user.getGoogleId(), "") && !user.getGoogleId().equals(socialDto.getGoogleId())) {
                        user.setGoogleId(socialDto.getGoogleId());
                    }
                    if (Objects.equals(user.getFacebookId(), "") && !user.getFacebookId().equals(socialDto.getFacebookId())) {
                        user.setFacebookId(socialDto.getFacebookId());
                    }
                })
                .switchIfEmpty(initSocialUser(Mono.just(socialDto)))
                .flatMap(userRepository::save);
    }

    private Mono<Boolean> isOldPasswordValid(String oldPassword) {
        return getCurrentUser()
                .map(user -> new BCryptPasswordEncoder().matches(oldPassword, user.getPassword()));
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<Boolean> updatePasswordOnRequest(Mono<PasswordUpdateDTO> updateDTOMono) {
        return updateDTOMono
                .filter(dto -> dto.getNewPassword().equals(dto.getNewPassword2()))
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.passwordsNotMatching())))
                .flatMap(dto -> isOldPasswordValid(dto.getOldPassword())
                        .filter(Boolean.TRUE::equals)
                        .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.currentPasswordIsNotValid())))
                        .flatMap(aBoolean -> getCurrentUser().flatMap(user -> updatePassword(user.getMail(), dto.getNewPassword())))
                        .map(aBoolean -> aBoolean));
    }

    public Mono<User> findByJustMail(String mail) {
        return userRepository.findByMail(mail);
    }

    public Mono<User> findByIdAndActiveTrue(String id) {
        return userRepository.findByIdAndActiveTrue(id);
    }

    public Mono<User> findByMail(String mail) {
        return userRepository.findByMailAndActiveTrue(mail);
    }

    public Mono<User> findByMailAndActiveFalse(String mail) {
        return userRepository.findByMailAndActiveFalse(mail);
    }

    public Mono<User> getCurrentUser() {
        return ReactiveSecurityContextHolder.getContext()
                .map(SecurityContext::getAuthentication)
                .map(Authentication::getName)
                .flatMap(userRepository::findByMailAndActiveTrue)
                .doOnError(ex -> {
                    throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, "Error while getting user data");
                });
    }

    private Mono<User> initSocialUser(Mono<SocialDTO> socialDTOMono) {
        return socialDTOMono.map(socialDTO -> {
            User user = new User();
            user.setFacebookId(socialDTO.getFacebookId());
            user.setGoogleId(socialDTO.getGoogleId());
            user.setMail(socialDTO.getMail());
            user.setName(socialDTO.getName());
            user.setSurname(socialDTO.getSurname());
            user.setPpUrl(socialDTO.getPpUrl());
            user.setActive(true);
            user.setRoles(new ArrayList<>(Collections
                    .singletonList(Role.builder().name(UserRoleEnum.ROLE_USER.name()).build())));
            return user;
        });
    }
}
