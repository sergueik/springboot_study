package com.pluralsight.websockets.message;

public class JoinMessage extends Message {
	private String name;

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}
}
