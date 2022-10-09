package example.api;


public enum StatusCode {
//@formatter:off
	OK(200), 
	CREATED(201), 
	ACCEPTED(202), 
	BAD_REQUEST(400), 
	METHOD_NOT_ALLOWED(405);
//@formatter:on
	private int code;

	private StatusCode(int code) {
		this.code = code;
	}

	public int getCode() {
		return code;
	}

}
