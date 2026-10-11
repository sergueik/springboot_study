package com.findinpath.unleash.config;

import com.findinpath.unleash.web.filter.BlueHeaderFilter;
import no.finn.unleash.Unleash;
import org.springframework.context.annotation.Bean;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.core.userdetails.User;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.provisioning.InMemoryUserDetailsManager;
import org.springframework.security.web.authentication.www.BasicAuthenticationFilter;

@EnableWebSecurity
public class SecurityConfiguration extends WebSecurityConfigurerAdapter {

    private final Unleash unleash;

    public SecurityConfiguration(Unleash unleash){
        this.unleash = unleash;
    }

    // @formatter:off
    @Override
    protected void configure(HttpSecurity http) throws Exception {
        http
                .authorizeRequests((authorize) -> authorize
                        .antMatchers("/css/**", "/index").permitAll()
                        .antMatchers("/user/**").hasRole("USER")
                )
                .formLogin((formLogin) -> formLogin
                        .loginPage("/login")
                        .failureUrl("/login-error")
                )
                .logout()
                .logoutSuccessUrl("/index");

        http.addFilterAfter(new BlueHeaderFilter(unleash), BasicAuthenticationFilter.class);
    }
    // @formatter:on

    @Bean
    public UserDetailsService userDetailsService() {
        UserDetails aliceDetails = User.withDefaultPasswordEncoder()
                .username("alice")
                .password("password")
                .roles("USER")
                .build();
        UserDetails bobDetails = User.withDefaultPasswordEncoder()
                .username("bob")
                .password("password")
                .roles("USER")
                .build();
        return new InMemoryUserDetailsManager(aliceDetails, bobDetails);
    }
}
