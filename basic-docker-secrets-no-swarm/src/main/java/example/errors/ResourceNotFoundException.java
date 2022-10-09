package example.errors;

@SuppressWarnings("serial")
class ResourceNotFoundException extends ApplicationException {
	ResourceNotFoundException(int code, String message) {
		super(code, message);
	}
}
