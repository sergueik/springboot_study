package com.bookportal.api.service;

import com.bookportal.api.entity.User;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class RegisterService {

    private final UserService userService;

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<User> createUser(Mono<User> user) {
        return userService.createUser(user);
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<User> saveGuest(Mono<User> user) {
        return userService.saveGuest(user);
    }


}
