package example.config;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.authentication.builders.AuthenticationManagerBuilder;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

@Configuration
@EnableWebSecurity
public class SecurityConfiguration extends WebSecurityConfigurerAdapter {

	@Value("${test.username}")
	private String username;

	@Value("${test.password}")
	private String password;

	@Autowired
	private BasicAuthenticationPoint basicAuthenticationPoint;

	@Override
	protected void configure(HttpSecurity http) throws Exception {
		http.csrf().disable();
		http.authorizeRequests().antMatchers("/welcome").permitAll();
		http.authorizeRequests().antMatchers("/svrinfo").permitAll();
		http.authorizeRequests().antMatchers("/upload").permitAll();
		http.authorizeRequests().antMatchers("/", "/api/**").permitAll()
				.anyRequest().authenticated();
		http.httpBasic().authenticationEntryPoint(basicAuthenticationPoint);

	}

	// https://spring.io/blog/2017/11/01/spring-security-5-0-0-rc1-released#password-storage-format
	// https://stackoverflow.com/questions/49654143/spring-security-5-there-is-no-passwordencoder-mapped-for-the-id-null
	@Autowired
	public void configureGlobal(AuthenticationManagerBuilder auth)
			throws Exception {
		System.err.println("Exect credentials: " + username + "/" + password);
		auth.inMemoryAuthentication().withUser(username)
				.password(String.format("{noop}%s", password)).roles("USER");
	}

}
