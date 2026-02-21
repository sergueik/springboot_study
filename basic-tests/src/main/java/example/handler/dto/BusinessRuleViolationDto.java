package example.handler.dto;

public class BusinessRuleViolationDto {

	private String type;
	private String title;
	private int status;
	private String detail;
	private String rule;

	public BusinessRuleViolationDto() {
	}

	public BusinessRuleViolationDto(String type, String title, int status, String detail, String rule) {
		this.type = type;
		this.title = title;
		this.status = status;
		this.detail = detail;
		this.rule = rule;
	}

	public String getType() {
		return type;
	}

	public String getTitle() {
		return title;
	}

	public int getStatus() {
		return status;
	}

	public String getDetail() {
		return detail;
	}

	public String getRule() {
		return rule;
	}

	public void setType(String type) {
		this.type = type;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	public void setDetail(String detail) {
		this.detail = detail;
	}

	public void setRule(String rule) {
		this.rule = rule;
	}
}