package example.api.user;

import java.io.IOException;

import java.io.InputStream;
import java.io.OutputStream;
import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.HttpExchange;

import example.api.Constants;
import example.api.Handler;
import example.api.ResponseEntity;
import example.api.StatusCode;
import example.domain.user.NewUser;
import example.domain.user.User;
import example.domain.user.UserService;
import example.errors.ApplicationExceptions;
import example.errors.GlobalExceptionHandler;

@SuppressWarnings("restriction")
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
		} else if ("GET".equals(exchange.getRequestMethod())) {
			// https://stackoverflow.com/questions/11640025/how-to-obtain-the-query-string-in-a-get-with-java-httpserver-httpexchange
			Map<String, String> queryParam = queryToMap(
					exchange.getRequestURI().getQuery());
			System.err.println("Processing query param: " + queryParam.keySet());
			ResponseEntity<User> e = doGet(queryParam.get("login"));
			System.err.println("Processing response: " + e.getBody().getLogin());
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

	private ResponseEntity<User> doGet(String login) {
		System.err.println("Getting user by login: " + login);
		User data = userService.get(login);
		System.err.println("Processing data: " + data.getLogin());
		return new ResponseEntity<User>(data,
				getHeaders(Constants.CONTENT_TYPE, Constants.APPLICATION_JSON),
				StatusCode.OK);
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

	public Map<String, String> queryToMap(String query) {
		if (query == null) {
			return null;
		}
		Map<String, String> result = new HashMap<>();
		for (String param : query.split("&")) {
			String[] entry = param.split("=");
			if (entry.length > 1) {
				result.put(entry[0], entry[1]);
			} else {
				result.put(entry[0], "");
			}
		}
		return result;
	}
}
