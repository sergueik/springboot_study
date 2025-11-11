package example.controller;

import java.util.Map;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.HttpStatusCode;
import org.springframework.http.ResponseEntity;
import org.springframework.http.converter.HttpMessageNotReadableException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.context.request.WebRequest;
import org.springframework.web.servlet.mvc.method.annotation.ResponseEntityExceptionHandler;

@ControllerAdvice
public class SanitizingExceptionHandler extends ResponseEntityExceptionHandler {

	private static final Logger logger = LoggerFactory.getLogger(SanitizingExceptionHandler.class);
	private static final Set<String> SENSITIVE = Set.of("email", "ssn", "password");

	@Override
	// NOTE: arg signature
	// SB 2.X HttpStatus
	// SB 3.X HttpStatusCode
	protected ResponseEntity<Object> handleHttpMessageNotReadable(HttpMessageNotReadableException e,
			HttpHeaders headers, HttpStatusCode statusCode, WebRequest request) {
		String sanitized = sanitizeExceptionMessage(e.getMessage());
		logger.warn("Malformed request (sanitized): {}", sanitized);
		return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(Map.of("error", "Malformed request"));
	}

	private String sanitizeExceptionMessage(String msg) {
		if (msg == null)
			return "";
		for (String k : SENSITIVE) {
			msg = msg.replaceAll("(?i)(\"" + k + "\"\\s*:\\s*\")[^\"]+(\")", "$1[REDACTED]$2");
		}
		return msg;
	}
}
