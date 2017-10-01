package example.secure.kaptcha.config.security;

import org.springframework.security.core.AuthenticationException;

public class KaptchaNotMatchException extends AuthenticationException {

	public KaptchaNotMatchException(String msg) {
		super(msg);
	}
}
