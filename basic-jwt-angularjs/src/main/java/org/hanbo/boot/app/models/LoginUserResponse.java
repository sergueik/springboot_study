package org.hanbo.boot.app.models;

public class LoginUserResponse extends LoginUser {
	private String authToken;

	public String getAuthToken() {
		return authToken;
	}

	public void setAuthToken(String authToken) {
		this.authToken = authToken;
	}
}
