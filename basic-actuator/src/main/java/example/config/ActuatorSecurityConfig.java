package example.config;

import org.springframework.boot.actuate.autoconfigure.security.servlet.EndpointRequest;
import org.springframework.boot.actuate.context.ShutdownEndpoint;
import org.springframework.boot.actuate.metrics.export.prometheus.PrometheusScrapeEndpoint;
import org.springframework.boot.autoconfigure.security.servlet.PathRequest;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;

// NOTE: commenting enables everything

@Configuration
public class ActuatorSecurityConfig extends WebSecurityConfigurerAdapter {

	/*
	 * Actuator 1.x
	 * 1. restrict "/shutdown"，to ACTUATOR_ADMIN role
	 * 2. allow external access to all static resources
	 * 3. allow external access '/'
	 * 4. require access explicitly otheriwise
	 * Actuator 2.x
	 * 1. Limit all access points ，to ACTUATOR_ADMIN role
	 * 2. allow external acces sto all static resources
	 * 3. allow external access '/'
	 * 4. require access explicitly otheriwise
	 */

	@Override
	protected void configure(HttpSecurity http) throws Exception {
		// Actuator 1.x
		// http
		// .authorizeRequests()
		// .requestMatchers(EndpointRequest.to(ShutdownEndpoint.class))
		// .hasRole("ACTUATOR_ADMIN")
		// .requestMatchers(EndpointRequest.toAnyEndpoint())
		// .permitAll()
		// .requestMatchers(PathRequest.toStaticResources().atCommonLocations())
		// .permitAll()
		// .antMatchers("/")
		// .permitAll()
		// .antMatchers("/**")
		// .authenticated()
		// .and()
		// .httpBasic();

		// Actuator 2.x

		//		http.authorizeRequests()
		//		.requestMatchers(EndpointRequest.toAnyEndpoint())
		//		.hasRole("ACTUATOR_ADMIN")
		//		.requestMatchers(PathRequest.toStaticResources().atCommonLocations())
		//		.permitAll().antMatchers("/").permitAll().antMatchers("/**")
		//		.authenticated().and().httpBasic();

		http.authorizeRequests().requestMatchers(EndpointRequest.toAnyEndpoint())
				.permitAll();
	}
}
