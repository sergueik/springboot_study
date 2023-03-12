package example.dto;

public class MessageDto {
	public MessageDto() {
		super();
	}

	public MessageDto(String body) {
		super();
		this.body = body;
	}

	public String getBody() {
		return body;
	}

	public void setBody(String body) {
		this.body = body;
	}

	private String body;
}
