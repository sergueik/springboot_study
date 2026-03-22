package org.hanbo.boot.app.security;

import java.util.HashMap;
import java.util.Map;

import org.hanbo.boot.app.models.LoginUser;
import org.hanbo.boot.app.models.UserRole;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;

@Component
public class MyJwtUserCheckService {
	private Map<String, LoginUser> _cachedLoginUsers;
	private Map<String, LoginUser> _cachedLoginUsersByUserName;
	@Value("${example.username}")
	private String username;
	@Value("${example.password}")
	private String password;

	public MyJwtUserCheckService() {

		_cachedLoginUsers = new HashMap<String, LoginUser>();
		_cachedLoginUsersByUserName = new HashMap<String, LoginUser>();

		LoginUser userToAdd = new LoginUser();
		userToAdd.setActive(true);
		userToAdd.setNickName("Test Admin");
		userToAdd.setUserEmail("testadmin-user@testthis.org");
		userToAdd.setUserId("0000000001");
		userToAdd.setUserPassword(password);
		userToAdd.setUserName(username);

		UserRole roleToAdd = new UserRole();
		roleToAdd.setRoleId("ROLE_ADMIN");
		roleToAdd.setRoleName("Admin User");
		userToAdd.addUserRole(roleToAdd);

		roleToAdd = new UserRole();
		roleToAdd.setRoleId("ROLE_STAFF");
		roleToAdd.setRoleName("Staff User");
		userToAdd.addUserRole(roleToAdd);

		roleToAdd = new UserRole();
		roleToAdd.setRoleId("ROLE_USER");
		roleToAdd.setRoleName("User");
		userToAdd.addUserRole(roleToAdd);

		_cachedLoginUsers.put(userToAdd.getUserId(), userToAdd);
		_cachedLoginUsersByUserName.put(userToAdd.getUserName(), userToAdd);

		userToAdd = new LoginUser();
		userToAdd.setActive(true);
		userToAdd.setNickName("Test Staff");
		userToAdd.setUserEmail("teststaff-user@testthis.org");
		userToAdd.setUserPassword("222test222");
		userToAdd.setUserId("0000000002");
		userToAdd.setUserName("teststaff");

		roleToAdd = new UserRole();
		roleToAdd.setRoleId("ROLE_STAFF");
		roleToAdd.setRoleName("Staff User");
		userToAdd.addUserRole(roleToAdd);

		roleToAdd = new UserRole();
		roleToAdd.setRoleId("ROLE_USER");
		roleToAdd.setRoleName("User");
		userToAdd.addUserRole(roleToAdd);

		_cachedLoginUsers.put(userToAdd.getUserId(), userToAdd);
		_cachedLoginUsersByUserName.put(userToAdd.getUserName(), userToAdd);

		userToAdd = new LoginUser();
		userToAdd.setActive(true);
		userToAdd.setNickName("Test User 1");
		userToAdd.setUserEmail("test-user1@testthis.org");
		userToAdd.setUserPassword("333test333");
		userToAdd.setUserId("0000000003");
		userToAdd.setUserName("testuser1");

		roleToAdd = new UserRole();
		roleToAdd.setRoleId("ROLE_USER");
		roleToAdd.setRoleName("User");
		userToAdd.addUserRole(roleToAdd);

		_cachedLoginUsers.put(userToAdd.getUserId(), userToAdd);
		_cachedLoginUsersByUserName.put(userToAdd.getUserName(), userToAdd);

		userToAdd = new LoginUser();
		userToAdd.setActive(false);
		userToAdd.setNickName("Test User 2");
		userToAdd.setUserEmail("test-user2@testthis.org");
		userToAdd.setUserPassword("444test444");
		userToAdd.setUserId("0000000004");
		userToAdd.setUserName("testuser2");

		roleToAdd = new UserRole();
		roleToAdd.setRoleId("ROLE_USER");
		roleToAdd.setRoleName("User");
		userToAdd.addUserRole(roleToAdd);

		_cachedLoginUsers.put(userToAdd.getUserId(), userToAdd);
		_cachedLoginUsersByUserName.put(userToAdd.getUserName(), userToAdd);
	}

	public LoginUser getUserDetails(String userId) {
		LoginUser retVal = null;
		if (!StringUtils.isEmpty(userId)) {
			if (_cachedLoginUsers.containsKey(userId)) {
				retVal = _cachedLoginUsers.get(userId);
			}
		}

		return retVal;
	}

	public LoginUser authenticateUser(String userName, String password) {
		LoginUser retVal = null;
		if (!StringUtils.isEmpty(userName)) {
			if (_cachedLoginUsersByUserName.containsKey(userName)) {
				System.out.println("found user key: " + userName);
				LoginUser foundUser = _cachedLoginUsersByUserName.get(userName);
				if (foundUser != null && !StringUtils.isEmpty(foundUser.getUserId()) && foundUser.isActive()) {
					System.out.println("found user: " + foundUser.getUserId());
					System.out.println("found user pass: " + foundUser.getUserPassword());
					if (!StringUtils.isEmpty(password) && password.equals(foundUser.getUserPassword())) {
						System.out.println("User match: " + foundUser.getUserId());
						retVal = foundUser;
					}
				} else {
					retVal = null;
				}
			}
		}

		return retVal;
	}
}
