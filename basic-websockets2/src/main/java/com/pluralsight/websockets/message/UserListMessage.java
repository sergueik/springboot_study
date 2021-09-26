package com.pluralsight.websockets.message;

import java.util.List;

import com.pluralsight.websockets.User;

public class UserListMessage extends Message {
	private List<User> users;

	public UserListMessage(List<User> users) {
		this.setType(MessageType.USERLIST);
		this.users = users;
	}

	public List<User> getUsers() {
		return users;
	}
}
