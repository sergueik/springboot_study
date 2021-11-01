package example.security;

import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;

@Configuration
@EnableWebSecurity
public class ApiConfigurerAdapter extends WebSecurityConfigurerAdapter {

	@Override
	protected void configure(HttpSecurity http) throws Exception {
		final String headerName = "API_KEY";
		ApiFilter filter = new ApiFilter(headerName);
		final String secret = "d3ebf20f-d202-4d9c-bcc9-80cb8de64901";
		filter.setAuthenticationManager(new ApiAuthenticationManager(secret));

		http.antMatcher("/api/v1/secure").csrf().disable().sessionManagement()
				.sessionCreationPolicy(SessionCreationPolicy.STATELESS).and()
				.addFilter(filter).authorizeRequests().anyRequest().authenticated();
	}
}
