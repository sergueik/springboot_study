package org.hanbo.boot.app.models;

import java.util.ArrayList;
import java.util.List;

public class LoginUser {
	private String userId;
	private String userName;
	private String nickName;
	private boolean active;
	private String userEmail;
	private String userPassword;

	private List<UserRole> allUserRoles;

	public LoginUser() {
		allUserRoles = new ArrayList<UserRole>();
	}

	public String getUserId() {
		return userId;
	}

	public void setUserId(String userId) {
		this.userId = userId;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getNickName() {
		return nickName;
	}

	public void setNickName(String nickName) {
		this.nickName = nickName;
	}

	public boolean isActive() {
		return active;
	}

	public void setActive(boolean active) {
		this.active = active;
	}

	public String getUserEmail() {
		return userEmail;
	}

	public void setUserEmail(String userEmail) {
		this.userEmail = userEmail;
	}

	public String getUserPassword() {
		return userPassword;
	}

	public void setUserPassword(String userPassword) {
		this.userPassword = userPassword;
	}

	public List<UserRole> getAllUserRoles() {
		return allUserRoles;
	}

	public void setAllUserRoles(List<UserRole> allUserRoles) {
		this.allUserRoles = allUserRoles;
	}

	public void addUserRole(UserRole roleToAdd) {
		if (roleToAdd != null) {
			boolean roleExists = allUserRoles.stream().anyMatch(x -> {
				return x.getRoleId().equalsIgnoreCase(roleToAdd.getRoleId());
			});
			if (!roleExists) {
				allUserRoles.add(roleToAdd);
			}
		}
	}
}
