package example.api;

import com.sun.net.httpserver.Headers;

@SuppressWarnings("restriction")
public class ResponseEntity<T> {
	private final T body;
	private final Headers headers;

	public T getBody() {
		return body;
	}

	public Headers getHeaders() {
		return headers;
	}

	public StatusCode getStatusCode() {
		return statusCode;
	}

	private final StatusCode statusCode;

	public ResponseEntity(T body, Headers headers, StatusCode statusCode) {
		this.body = body;
		this.headers = headers;
		this.statusCode = statusCode;
	}
}