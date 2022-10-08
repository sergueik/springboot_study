package example.errors;

class ResourceNotFoundException extends ApplicationException {
	ResourceNotFoundException(int code, String message) {
		super(code, message);
	}
}
