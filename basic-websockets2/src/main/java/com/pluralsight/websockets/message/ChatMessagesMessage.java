package com.pluralsight.websockets.message;

import java.util.List;

public class ChatMessagesMessage extends Message {
	private List<ChatMessage> messages;

	public ChatMessagesMessage(List<ChatMessage> messages) {
		super();
		this.setType(MessageType.MESSAGELIST);
		this.messages = messages;
	}

	public List<ChatMessage> getMessages() {
		return messages;
	}
}
