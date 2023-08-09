
package com.pocisoft.eventsapi.security;

import java.util.ArrayList;
import java.util.Collection;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.userdetails.User;

import com.pocisoft.eventsapi.data.UserAccount;

public class CustomUserDetails extends User {
    /**
	 * 
	 */
	private static final long serialVersionUID = -3133043397211672639L;
	private final int userId;

    public CustomUserDetails(String username, String password, int userId) {
        super(username, password, new ArrayList<>());
        this.userId = userId;
    }

    public static CustomUserDetails fromUserAccount(UserAccount userAccount) {
        
        return new CustomUserDetails(userAccount.getEmail(), userAccount.getPassword(), userAccount.getId());
    }

    public int getUserId() {
        return userId;
    }
}