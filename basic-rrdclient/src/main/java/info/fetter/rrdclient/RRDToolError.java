package info.fetter.rrdclient;

@SuppressWarnings("serial")
public class RRDToolError extends RuntimeException {
	public RRDToolError(String message) {
		super(message);
	}
}
