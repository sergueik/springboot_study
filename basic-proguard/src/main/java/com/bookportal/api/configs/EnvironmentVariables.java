package com.bookportal.api.configs;

import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.PropertySource;
import org.springframework.context.annotation.PropertySources;
import org.springframework.core.env.Environment;
import org.springframework.stereotype.Component;

@Component
@RequiredArgsConstructor
@PropertySources({
        @PropertySource(value = "classpath:messages.yml", encoding = "UTF-8"),
})
public class EnvironmentVariables {
    private final Environment environment;

    public String eMailAlreadyInUse() {
        return environment.getProperty("mail-already-in-use");
    }

    public String tokenUserNotValid() {
        return environment.getProperty("token-mail-no-match");
    }

    public String incorrectCredentials() {
        return environment.getProperty("incorrect-credentials");
    }

    public String userInactive() {
        return environment.getProperty("inactive-user");
    }

    public String facebookIdAlreadyInUse() {
        return environment.getProperty("facebookId-already-in-use");
    }

    public String googleIdAlreadyInUse() {
        return environment.getProperty("googleId-already-in-use");
    }

    public String emailValidityExpired() {
        return environment.getProperty("validity-expired");
    }

    public String userAlreadyConfirmedEmail() {
        return environment.getProperty("already-confirmed-email");
    }

    public String currentPasswordIsNotValid() {
        return environment.getProperty("current-password-not-valid");
    }

    public String passwordsNotMatching() {
        return environment.getProperty("passwords-not-matching");
    }

    public String lessThanOne() {
        return environment.getProperty("cant-be-lesser-than-1");
    }

    public String greaterThanFive() {
        return environment.getProperty("cant-be-greater-than-5");
    }

    public String invalidFacebookId() {
        return environment.getProperty("invalid-facebook-id");
    }

    public String invalidGoogleId() {
        return environment.getProperty("invalid-google-id");
    }

    public String userCouldNotAuthenticated() {
        return environment.getProperty("user-could-not-auth");
    }

    public String passwordResetMailSubject() {
        return environment.getProperty("mail-password-reset");
    }

    public String verifyAccountMailSubject() {
        return environment.getProperty("mail-verify-account");
    }

    public String mailFrom() {
        return environment.getProperty("mail-from");
    }

    public String invalidEnvironment() {
        return environment.getProperty("invalid-environment");
    }

    public String environmentNotFound() {
        return environment.getProperty("environment-not-found");
    }

    public String invalidVersion() {
        return environment.getProperty("invalid-version");
    }

    public String userNotFound() {
        return environment.getProperty("user-not-found");
    }

    public String mailWillBeSent() {
        return environment.getProperty("mail-will-be-sent");
    }

}


