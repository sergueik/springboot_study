package com.bookportal.api.service;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.entity.PasswordReset;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.PasswordResetRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import java.util.Date;
import java.util.UUID;

@Service
@RequiredArgsConstructor
public class PasswordResetService {
    private final UserService userService;
    private final EmailService emailService;
    public final static Long DAY = 1000L * 60 * 60 * 24;
    private final PasswordResetRepository passwordResetRepository;
    private final EnvironmentVariables env;

    public Mono<PasswordReset> generatePasswordResetKey(String email) {
        return userService.findByMail(email)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.USER.getValue())))
                .flatMap(user -> {
                    PasswordReset passResetObj = new PasswordReset();
                    passResetObj.setUser(user);
                    passResetObj.setSecretKey(generateRandomKey());
                    passResetObj.setActive(true);
                    passResetObj.setValidity(new Date(System.currentTimeMillis() + DAY));
                    return passwordResetRepository.save(passResetObj)
                            .doOnNext(emailService::sendPasswordResetLink);
                });
    }

    public Mono<Boolean> isValidKey(String key, String mail) {
        return passwordResetRepository.findBySecretKeyAndActiveTrue(key)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.emailValidityExpired())))
                .map(passwordReset -> mail.equals(passwordReset.getUser().getMail()) && new Date().before(passwordReset.getValidity()));
    }

    private String generateRandomKey() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public Mono<Boolean> updateUserKey(String key) {
        return passwordResetRepository.findBySecretKeyAndActiveTrue(key)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.NOT_FOUND, "Key not found")))
                .map(passwordReset -> {
                    passwordReset.setActive(false);
                    return passwordReset;
                })
                .flatMap(passwordResetRepository::save)
                .map(passwordReset -> !passwordReset.isActive());
    }
}
