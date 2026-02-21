package example.handler.dto;

public class BusinessError {

	private String rule;
	private String message;

	public BusinessError(String rule, String message) {
		this.rule = rule;
		this.message = message;
	}

	public String getRule() {
		return rule;
	}

	public String getMessage() {
		return message;
	}
}