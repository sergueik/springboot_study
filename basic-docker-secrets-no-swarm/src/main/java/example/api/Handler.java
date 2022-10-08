package example.api;

import java.io.InputStream;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.sun.net.httpserver.Headers;
import com.sun.net.httpserver.HttpExchange;

import example.errors.ApplicationExceptions;
import example.errors.GlobalExceptionHandler;
import io.vavr.control.Try;

public abstract class Handler {

	private final ObjectMapper objectMapper;
	private final GlobalExceptionHandler exceptionHandler;

	public Handler(ObjectMapper objectMapper,
			GlobalExceptionHandler exceptionHandler) {
		this.objectMapper = objectMapper;
		this.exceptionHandler = exceptionHandler;
	}

	public void handle(HttpExchange exchange) {
		Try.run(() -> execute(exchange))
				.onFailure(thr -> exceptionHandler.handle(thr, exchange));
	}

	protected abstract void execute(HttpExchange exchange) throws Exception;

	protected <T> T readRequest(InputStream is, Class<T> type) {
		return Try.of(() -> objectMapper.readValue(is, type))
				.getOrElseThrow(ApplicationExceptions.invalidRequest());
	}

	protected <T> byte[] writeResponse(T response) {
		return Try.of(() -> objectMapper.writeValueAsBytes(response))
				.getOrElseThrow(ApplicationExceptions.invalidRequest());
	}

	protected static Headers getHeaders(String key, String value) {
		Headers headers = new Headers();
		headers.set(key, value);
		return headers;
	}
}
