package example.security;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import example.data.UserAccount;
import example.data.UsersDAO;

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