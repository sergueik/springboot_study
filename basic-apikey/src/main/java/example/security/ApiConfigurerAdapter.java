package example.security;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;

@Configuration
@EnableWebSecurity
public class ApiConfigurerAdapter extends WebSecurityConfigurerAdapter {
	@Value("${header:API_KEY}")
	private String headerName;
	@Value("${secret}")
	private String secret;

	@Override
	protected void configure(HttpSecurity http) throws Exception {
		ApiFilter filter = new ApiFilter(headerName);
		filter.setAuthenticationManager(new ApiAuthenticationManager(secret));

		http.antMatcher("/api/v1/secure").csrf().disable().sessionManagement()
				.sessionCreationPolicy(SessionCreationPolicy.STATELESS).and()
				.addFilter(filter).authorizeRequests().anyRequest().authenticated();
	}
}
