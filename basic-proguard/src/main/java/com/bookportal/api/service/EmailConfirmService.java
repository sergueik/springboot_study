package com.bookportal.api.service;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.entity.EmailConfirm;
import com.bookportal.api.entity.User;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.repository.EmailConfirmRepository;
import com.bookportal.api.util.mapper.UserMapper;
import lombok.AllArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import java.util.Date;
import java.util.UUID;

@Service
@AllArgsConstructor
public class EmailConfirmService {
    private final EmailConfirmRepository emailConfirmRepository;
    private final EnvironmentVariables env;

    public Mono<EmailConfirm> generateEmailConfirmationKey(User user) {
        return emailConfirmRepository.findByUser_IdAndActiveTrue(user.getId())
                .defaultIfEmpty(initIfEmpty(user))
                .flatMap(emailConfirmRepository::save);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<EmailConfirm> updateUserKeyToInactive(String key) {
        return emailConfirmRepository.findBySecretKeyAndActiveTrue(key)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.KEY.getValue())))
                .flatMap(emailConfirm -> {
                    emailConfirm.setActive(false);
                    return emailConfirmRepository.save(emailConfirm);
                });
    }

    public Mono<EmailConfirm> findBySecretKeyAndActiveTrue(String key) {
        return emailConfirmRepository.findBySecretKeyAndActiveTrue(key)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.KEY.getValue())))
                .filter(emailConfirm -> new Date().before(emailConfirm.getValidUntil()))
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.emailValidityExpired())));
    }

    private String generateRandomKey() {
        return UUID.randomUUID().toString().replace("-", "");
    }

    private EmailConfirm initIfEmpty(User user) {
        EmailConfirm emailConfirm = new EmailConfirm();
        emailConfirm.setUser(UserMapper.userToSoftUser(user));
        emailConfirm.setSecretKey(generateRandomKey());
        return emailConfirm;
    }

}
