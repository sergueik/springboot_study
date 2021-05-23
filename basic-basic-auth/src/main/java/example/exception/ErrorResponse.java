package example.exception;

import java.util.List;

public class ErrorResponse {
	public ErrorResponse(String message, List<String> details) {
		super();
		this.message = message;
		this.details = details;
	}

	// General error message about nature of error
	private String message;

	// Specific errors in API request processing
	private List<String> details;

	public String getMessage() {
		return message;
	}

	public void setMessage(String data) {
		message = data;
	}

	public List<String> getDetails() {
		return details;
	}

	public void setDetails(List<String> data) {
		details = data;
	}
}
