package example.data;

@SuppressWarnings("serial")
public class APIException extends RuntimeException {
	private int statusCode;

	public APIException(int statusCode, String errorMessage) {
		super(errorMessage);
		this.statusCode = statusCode;
	}

	public int getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(int statusCode) {
		this.statusCode = statusCode;
	}
}
