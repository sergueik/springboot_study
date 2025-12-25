package com.bookportal.api.controllers.common;

import com.bookportal.api.auth.JwtAuthenticationManager;
import com.bookportal.api.auth.JwtTokenProvider;
import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.entity.Role;
import com.bookportal.api.entity.User;
import com.bookportal.api.model.*;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.RegisterService;
import com.bookportal.api.service.UserService;
import io.swagger.annotations.ApiOperation;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.HttpStatus;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.authentication.DisabledException;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import javax.validation.Valid;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/v1/login")
@RequiredArgsConstructor
@Slf4j
public class LoginController {
    private final JwtAuthenticationManager authenticationManager;
    private final JwtTokenProvider jwtUtil;
    private final RegisterService registerService;
    private final EnvironmentVariables env;
    private final UserService userService;

    @PostMapping
    @ApiOperation(value = "login")
    public Mono<CustomResponse> createAuthenticationToken(@Valid @RequestBody Mono<UserDTO> userDTOMono) {
        return userDTOMono
                .flatMap(userDTO -> userService.findByJustMail(userDTO.getMail())
                        .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.userNotFound())))
                        .flatMap(user -> {
                            if (!new BCryptPasswordEncoder().matches(userDTO.getPassword(), user.getPassword())) {
                                return Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.incorrectCredentials()));
                            }
                            if (user.isActive()) {
                                return setJwtResponse(userDTO.getMail());
                            }
                            return Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.userInactive()));
                        })
                        .doOnNext(jwtResponseDTO -> authenticateUser(jwtResponseDTO.getUsername(), jwtResponseDTO.getToken()))
                        .map(CustomResponse::responseOk));
    }

    @PostMapping("/social")
    @ApiOperation(value = "social login")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    public Mono<CustomResponse> createSocialAuthenticationToken(@Valid @RequestBody Mono<SocialDTO> dto) {
        return dto.flatMap(userService::createSocialUser)
                .flatMap(user -> setJwtResponse(user.getMail()))
                .map(CustomResponse::responseOk);
    }

    @PostMapping("/refresh")
    @ApiOperation(value = "refresh token")
    public Mono<CustomResponse> refreshAuthenticationToken(@Valid @RequestBody Mono<JwtRefreshDTO> dtoMono) {
        return dtoMono.doOnNext(jwtRefreshDTO -> {
                    final String username = jwtUtil.getUsernameFromToken(jwtRefreshDTO.getToken());
                    if (!username.equals(jwtRefreshDTO.getMail())) {
                        throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, env.tokenUserNotValid());
                    }
                })
                .flatMap(jwtRefreshDTO -> setJwtResponse(jwtRefreshDTO.getMail()))
                .map(CustomResponse::responseOk);


    }

    @PostMapping("/guest")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "create guest user")
    public Mono<CustomResponse> createGuestToken(@Valid @RequestBody Mono<GuestDTO> guestDTOMono) {
        return guestDTOMono
                .map(guestDTO -> {
                    User user = new User();
                    user.setMail(guestDTO.getAndroidID());
                    return Mono.just(user);
                })
                .flatMap(registerService::saveGuest)
                .map(user -> setJwtResponse(user.getMail()))
                .flatMap(mono -> mono.map(CustomResponse::responseOk));
    }

    private void authenticateUser(String username, String password) {
        try {
            authenticationManager.authenticate(new UsernamePasswordAuthenticationToken(username, password));
        } catch (DisabledException e) {
            throw new ResponseStatusException(HttpStatus.FORBIDDEN, env.userInactive());
        } catch (BadCredentialsException e) {
            throw new ResponseStatusException(HttpStatus.UNAUTHORIZED, env.incorrectCredentials());
        }
    }

    public Mono<JwtResponseDTO> setJwtResponse(String mail) {
        DateFormat formatter = new SimpleDateFormat("dd.MM.yyyy HH:mm:ss");
        return userService.findByMail(mail)
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.FORBIDDEN, env.userCouldNotAuthenticated())))
                .map(user -> {
                    final String token = jwtUtil.generateToken(user.getMail(), user.getRoles());
                    final String username = jwtUtil.getUsernameFromToken(token);
                    final String issuedAt = formatter.format(new Date());
                    final String expireAt = formatter.format(jwtUtil.getExpirationDateFromToken(token));
                    return getJwtResponseDTO(token, username, issuedAt, expireAt, user);
                });
    }

    private JwtResponseDTO getJwtResponseDTO(String token, String username, String issuedAt, String expireAt, User user) {
        JwtResponseDTO dto = new JwtResponseDTO();
        dto.setStatus(HttpStatus.OK.value());
        dto.setUsername(username);
        dto.setPpUrl(user.getPpUrl());
        dto.setIssuedAt(issuedAt);
        dto.setExpireAt(expireAt);
        dto.setName(user.getName());
        dto.setSurname(user.getSurname());
        dto.setToken(token);
        dto.setUserId(user.getId());
        dto.setRoles(user.getRoles().stream().map(Role::getName).collect(Collectors.toList()));
        return dto;
    }
}
