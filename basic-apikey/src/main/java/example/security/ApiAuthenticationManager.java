package example.security;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.BadCredentialsException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;

import example.controller.SecureController;

public class ApiAuthenticationManager implements AuthenticationManager {
	private static final Logger logger = LoggerFactory
			.getLogger(ApiAuthenticationManager.class);

	private final String secret;

	public ApiAuthenticationManager(String value) {
		secret = value;
	}

	@Override
	public Authentication authenticate(Authentication authentication)
			throws AuthenticationException {
		logger.info("authenticate request with key: "
				+ authentication.getPrincipal().toString());

		if (!secret.equalsIgnoreCase(authentication.getPrincipal().toString())) {
			logger.info("Invalid api key or secret");
			throw new BadCredentialsException(null);
			// the message is not shown to the user anyway
		} else {
			authentication.setAuthenticated(true);
			return authentication;
		}
	}
}
