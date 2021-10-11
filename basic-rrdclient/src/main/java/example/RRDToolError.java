package example;

@SuppressWarnings("serial")
public class RRDToolError extends RuntimeException {
	public RRDToolError(String message) {
		super(message);
	}
}
