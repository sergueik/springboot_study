package example.api.user;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;

import example.api.Constants;
import example.api.Handler;
import example.api.ResponseEntity;
import example.api.StatusCode;
import example.domain.user.NewUser;
import example.domain.user.UserService;
import example.errors.ApplicationExceptions;
import example.errors.GlobalExceptionHandler;

public class RegistrationHandler extends Handler {

	private final UserService userService;

	public RegistrationHandler(UserService userService, ObjectMapper objectMapper,
			GlobalExceptionHandler exceptionHandler) {
		super(objectMapper, exceptionHandler);
		this.userService = userService;
	}

	@Override
	protected void execute(HttpExchange exchange) throws IOException {
		byte[] response;
		if ("POST".equals(exchange.getRequestMethod())) {
			ResponseEntity e = doPost(exchange.getRequestBody());
			exchange.getResponseHeaders().putAll(e.getHeaders());
			exchange.sendResponseHeaders(e.getStatusCode().getCode(), 0);
			response = super.writeResponse(e.getBody());
		} else {
			throw ApplicationExceptions
					.methodNotAllowed("Method " + exchange.getRequestMethod()
							+ " is not allowed for " + exchange.getRequestURI())
					.get();
		}

		OutputStream os = exchange.getResponseBody();
		os.write(response);
		os.close();
	}

	private ResponseEntity<RegistrationResponse> doPost(InputStream is) {
		RegistrationRequest registerRequest = super.readRequest(is,
				RegistrationRequest.class);

		NewUser user = NewUser.builder().login(registerRequest.getLogin())
				.password(PasswordEncoder.encode(registerRequest.getPassword()))
				.build();

		String userId = userService.create(user);

		RegistrationResponse response = new RegistrationResponse(userId);

		return new ResponseEntity<>(response,
				getHeaders(Constants.CONTENT_TYPE, Constants.APPLICATION_JSON),
				StatusCode.OK);
	}
}
