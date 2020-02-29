package example.model;

// based on: 
public class ChatMessage {
	private MessageType type;
	private String content;
	private String sender;

	public enum MessageType {
		CHAT, JOIN, LEAVE
	}

	public MessageType getType() {
		return type;
	}

	public void setType(MessageType data) {
		type = data;
	}

	public String getContent() {
		return content;
	}

	public void setContent(String data) {
		content = data;
	}

	public String getSender() {
		return sender;
	}

	public void setSender(String data) {
		sender = data;
	}
}
