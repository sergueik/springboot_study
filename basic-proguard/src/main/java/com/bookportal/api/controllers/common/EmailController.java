package com.bookportal.api.controllers.common;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.entity.User;
import com.bookportal.api.exception.CustomNotFoundException;
import com.bookportal.api.model.PasswordResetDTO;
import com.bookportal.api.model.enums.ExceptionItemsEnum;
import com.bookportal.api.response.CustomResponse;
import com.bookportal.api.service.EmailConfirmService;
import com.bookportal.api.service.EmailService;
import com.bookportal.api.service.PasswordResetService;
import com.bookportal.api.service.UserService;
import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiParam;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

import javax.validation.Valid;
import java.util.Optional;

@RestController
@RequestMapping("/sendMail")
@RequiredArgsConstructor
public class EmailController {
    private final UserService userService;
    private final EmailService emailService;
    private final PasswordResetService passwordResetService;
    private final EmailConfirmService emailConfirmService;
    private final EnvironmentVariables env;
    private final LoginController loginController;

    @GetMapping("/resetPassword")
    @ApiOperation(value = "Send password reset e-mail")
    public Mono<CustomResponse> resetPassword(
            @ApiParam(required = true)
            @RequestParam("email") String email) {
        return passwordResetService.generatePasswordResetKey(email)
                .doOnError(throwable -> Mono.just(CustomResponse.responseOk(env.mailWillBeSent())))
                .map(passwordReset -> CustomResponse.responseOk(env.mailWillBeSent()));
    }

    @PostMapping("/resetPassword")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Reset password by key and email")
    public Mono<CustomResponse> updatePassword(@Valid @RequestBody Mono<PasswordResetDTO> dtoMono) {
        return dtoMono
                .flatMap(passwordResetDTO -> userService.findByMail(passwordResetDTO.getEmail())
                        .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.USER.getValue())))
                        .flatMap(user -> passwordResetService.isValidKey(passwordResetDTO.getKey(), user.getMail()))
                        .filter(Boolean.TRUE::equals)
                        .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, "Invalid key")))
                        .flatMap(aBoolean -> passwordResetService.updateUserKey(passwordResetDTO.getKey())
                                .flatMap(aBoolean1 -> userService.updatePassword(passwordResetDTO.getEmail(), passwordResetDTO.getNewPass()))))
                .map(CustomResponse::responseOk);
    }

    @GetMapping("/confirmEmail")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Confirm account")
    public Mono<CustomResponse> confirmEmail(
            @ApiParam(required = true) @RequestParam("email") String email,
            @ApiParam(required = true) @RequestParam("key") String key) {
        return userService.findByJustMail(email)
                .switchIfEmpty(Mono.error(new CustomNotFoundException(ExceptionItemsEnum.USER.getValue())))
                .filter(user -> Boolean.FALSE.equals(user.isActive()))
                .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, env.userAlreadyConfirmedEmail())))
                .flatMap(user -> emailConfirmService.findBySecretKeyAndActiveTrue(key)
                        .flatMap(emailConfirm -> userService.setUserToActive(emailConfirm.getUser().getId()))
                        .flatMap(user1 -> emailConfirmService.updateUserKeyToInactive(key))
                        .map(CustomResponse::responseOk)
                        .switchIfEmpty(Mono.just(CustomResponse.responseOk(true))));
    }

    @GetMapping("/resendConfirmationEmail")
    @Transactional(propagation = Propagation.REQUIRES_NEW, rollbackFor = Exception.class)
    @ApiOperation(value = "Send confirmation email again")
    public Mono<CustomResponse> resendConfirmationEmail(
            @ApiParam(required = true) @RequestParam("email") String email) {
        return userService.findByMailAndActiveFalse(email)
                .doOnNext(emailService::sendEmailConfirmationLink)
                .defaultIfEmpty(new User())
                .map(user -> CustomResponse.responseOk(env.mailWillBeSent()));
    }
}
