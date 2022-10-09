package example.api;

public class ErrorResponse {
	int code;
	String message;

	public int getCode() {
		return code;
	}

	public String getMessage() {
		return message;
	}

	private ErrorResponse(ErrorResponseBuilder builder) {
		this.code = builder.code;
		this.message = builder.message;
	}

	public static ErrorResponseBuilder builder() {
		return new ErrorResponseBuilder();
	}

	// Builder Class
	public static class ErrorResponseBuilder {
		// required parameters
		int code;
		String message;

		public ErrorResponseBuilder() {

		}

		public ErrorResponseBuilder(int code, String message) {
			this.code = code;
			this.message = message;
		}

		public ErrorResponseBuilder code(int code) {
			this.code = code;
			return this;
		}

		public ErrorResponseBuilder message(String message) {
			this.message = message;
			return this;
		}

		public ErrorResponse build() {
			return new ErrorResponse(this);
		}
	}

}
