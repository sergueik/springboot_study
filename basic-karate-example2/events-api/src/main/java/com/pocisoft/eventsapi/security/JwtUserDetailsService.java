package com.pocisoft.eventsapi.security;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import com.pocisoft.eventsapi.data.UserAccount;
import com.pocisoft.eventsapi.data.UsersDAO;

@Service
public class JwtUserDetailsService implements UserDetailsService {

	@Override
	public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
		
		 UserAccount user =  new UsersDAO().getUserByUserName(username);
	        if (user == null) {
	            throw new UsernameNotFoundException("User not found with username: " + username);
	        } else {
	           
	            return CustomUserDetails.fromUserAccount(user);
	        }
	}
}