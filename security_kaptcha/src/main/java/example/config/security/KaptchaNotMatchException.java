package example.config.security;

import org.springframework.security.core.AuthenticationException;

@SuppressWarnings("serial")
public class KaptchaNotMatchException extends AuthenticationException {

	public KaptchaNotMatchException(String msg) {
		super(msg);
	}
}
