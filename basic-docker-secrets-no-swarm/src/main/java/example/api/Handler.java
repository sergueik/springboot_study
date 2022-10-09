package example.api;

import java.io.InputStream;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;

import example.errors.ApplicationExceptions;
import example.errors.GlobalExceptionHandler;

public abstract class Handler {

	private final ObjectMapper objectMapper;
	private final GlobalExceptionHandler exceptionHandler;

	public Handler(ObjectMapper objectMapper,
			GlobalExceptionHandler exceptionHandler) {
		this.objectMapper = objectMapper;
		this.exceptionHandler = exceptionHandler;
	}

	public void handle(HttpExchange exchange) {
		try {
			execute(exchange);
		} catch (Exception e) {
			exceptionHandler.handle(e, exchange);
		}
	}

	protected abstract void execute(HttpExchange exchange) throws Exception;

	protected <T> T readRequest(InputStream is, Class<T> type) {
		T result = null;
		try {
			result = objectMapper.readValue(is, type);
		} catch (Exception e) {
			throw ApplicationExceptions.invalidRequest().apply(e);
		}
		return result;
	}

	protected <T> byte[] writeResponse(T response) {
		byte[] result = null;
		try {
			result = objectMapper.writeValueAsBytes(response);
		} catch (Exception e) {
			throw ApplicationExceptions.invalidRequest().apply(e);
		}
		return result;
	}

	@SuppressWarnings("restriction")
	protected static Headers getHeaders(String key, String value) {
		Headers headers = new Headers();
		headers.set(key, value);
		return headers;
	}
}
