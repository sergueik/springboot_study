package com.bookportal.api.configs;

import com.bookportal.api.auth.AfterRequestFilter;
import com.bookportal.api.auth.JwtAuthenticationManager;
import com.bookportal.api.auth.JwtRequestFilter;
import lombok.RequiredArgsConstructor;
import org.springframework.context.annotation.Bean;
import org.springframework.security.config.annotation.method.configuration.EnableReactiveMethodSecurity;
import org.springframework.security.config.annotation.web.reactive.EnableWebFluxSecurity;
import org.springframework.security.config.web.server.SecurityWebFiltersOrder;
import org.springframework.security.config.web.server.ServerHttpSecurity;
import org.springframework.security.crypto.factory.PasswordEncoderFactories;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.server.SecurityWebFilterChain;
import org.springframework.security.web.server.authentication.AuthenticationWebFilter;

@EnableWebFluxSecurity
@EnableReactiveMethodSecurity
@RequiredArgsConstructor
public class WebFluxSecurityConfig {
    private final JwtRequestFilter jwtRequestFilter;
    private final AfterRequestFilter afterRequestFilter;
    private final JwtAuthenticationManager jwtAuthenticationManager;
    private static final String[] AUTH_WHITELIST = {
            "/api/v1/register",
            //
            "/api/v1/home",
            //
            "/api/v1/version",
            //
            "/api/v1/login",
            "/api/v1/login/guest",
            "/api/v1/login/refresh",
            "/api/v1/login/social",
            //
            "/v2/api-docs",
            //
            "/sendMail/**",
            //
            "/favicon.ico"
    };

    @Bean
    public SecurityWebFilterChain springSecurityFilterChain(ServerHttpSecurity http) {
        ServerHttpSecurity httpSecurity = http
                .cors().disable()
                .httpBasic().disable()
                .formLogin().disable()
                .csrf().disable()
                .logout().disable()
                .authorizeExchange()
                .pathMatchers("/api/v1/admin", "/api/v1/admin/**").hasRole("ADMIN")
                .pathMatchers(AUTH_WHITELIST).permitAll()
                .anyExchange().authenticated()
                .and()
                .addFilterBefore(jwtRequestFilter, SecurityWebFiltersOrder.AUTHENTICATION)
                .addFilterBefore(authenticationWebFilter(), SecurityWebFiltersOrder.AUTHENTICATION)
                .addFilterAfter(afterRequestFilter, SecurityWebFiltersOrder.LAST);
        return httpSecurity.build();
    }

    @Bean
    public PasswordEncoder passwordEncoder() {
        return PasswordEncoderFactories.createDelegatingPasswordEncoder();
    }

    private AuthenticationWebFilter authenticationWebFilter() {
        return new AuthenticationWebFilter(jwtAuthenticationManager);
    }
}