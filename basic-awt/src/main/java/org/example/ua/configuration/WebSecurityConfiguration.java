package org.example.ua.configuration;

import org.example.ua.security.JwtTokenFilter;
import org.example.ua.security.JwtTokenProvider;
import org.example.ua.security.SecurityOffFilter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpMethod;
import org.springframework.security.config.annotation.method.configuration.EnableGlobalMethodSecurity;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.builders.WebSecurity;
import org.springframework.security.config.annotation.web.configuration.WebSecurityConfigurerAdapter;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

@Configuration
@EnableGlobalMethodSecurity(prePostEnabled = true)
public class WebSecurityConfiguration extends WebSecurityConfigurerAdapter {

    @Value("${application.develop-mode}")
    private boolean developMode;

    @Value("${application.admin}")
    private String adminEmail;

    @Autowired
    private JwtTokenProvider jwtTokenProvider;

    @Override
    protected void configure(HttpSecurity http) throws Exception {
        // Disable CSRF (cross site request forgery)
        http.csrf().disable();

        // No session will be created or used by spring security
        http.sessionManagement().sessionCreationPolicy(SessionCreationPolicy.STATELESS);

        // Entry points
        http.authorizeRequests()//
                .antMatchers("/user/signin/**").permitAll()
                .antMatchers("/user/register/**").permitAll()
                // Disallow everything else...
                .anyRequest().authenticated();

        if (developMode) {
            SecurityOffFilter filter = new SecurityOffFilter(adminEmail, jwtTokenProvider);
            http.addFilterBefore(filter, UsernamePasswordAuthenticationFilter.class);
            return;
        }
        // Apply JWT
        JwtTokenFilter filter = new JwtTokenFilter(jwtTokenProvider);
        http.addFilterBefore(filter, UsernamePasswordAuthenticationFilter.class);
    }

    @Override
    public void configure(WebSecurity web) {
        web
                .ignoring()
                //Allow swagger
                .antMatchers(
                        "/v2/api-docs",
                        "/swagger-resources/**",
                        "/swagger-ui.html",
                        "/webjars/**"
                )
                //Allow CORS
                .antMatchers(HttpMethod.OPTIONS);
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }

}
