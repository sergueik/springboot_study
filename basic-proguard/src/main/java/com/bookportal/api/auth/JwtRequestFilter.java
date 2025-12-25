package com.bookportal.api.auth;

import com.bookportal.api.exception.CustomUnauthorizedException;
import com.bookportal.api.repository.UserRepository;
import io.jsonwebtoken.ExpiredJwtException;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
//the lombok.extern.slf4j.Sld4j does not work.
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.context.ReactiveSecurityContextHolder;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;

@Component
@RequiredArgsConstructor
@Slf4j
public class JwtRequestFilter implements WebFilter {
	private final Logger log = LoggerFactory.getLogger(JwtRequestFilter.class);
	private final JwtTokenProvider tokenProvider;
    private final MyReactiveUserDetailsService jwtUserDetailsService;
    private final UserRepository userRepository;
    private static final String[] AUTH_WHITELIST = {
            "/api/v1/register",
            "/api/v1/login",
            "/api/v1/login/social",
            "/api/v1/login/refresh",
            "/api/v1/login/guest",
    };

    @Override
    public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {
        String path = exchange.getRequest().getPath().toString();
        boolean contains = Arrays.asList(AUTH_WHITELIST).contains(path);
        if (!contains) {
            List<String> authorization = exchange.getRequest().getHeaders().get("Authorization");
            if (authorization != null && !authorization.isEmpty()) {
                String requestTokenHeader = authorization.get(0);
                String username = null;
                String jwtToken = null;
                if (requestTokenHeader != null && requestTokenHeader.startsWith("Bearer ")) {
                    jwtToken = requestTokenHeader.substring(7);
                    try {
                        username = tokenProvider.getUsernameFromToken(jwtToken);
                    } catch (IllegalArgumentException e) {
                        log.warn("Unable to get JWT Token.");
                        return Mono.error(new CustomUnauthorizedException("Unable to get JWT Token."));
                    } catch (ExpiredJwtException e) {
                        log.warn("JWT Token has expired.");
                        return Mono.error(new CustomUnauthorizedException("JWT Token has expired."));
                    }
                }

                if (username != null && SecurityContextHolder.getContext().getAuthentication() == null) {
                    String finalJwtToken = jwtToken;
                    return userRepository.findByMailAndActiveTrue(username)
                            .flatMap(user -> jwtUserDetailsService.findByUsername(user.getMail()))
                            .flatMap(userDetails -> {
                                Boolean aBoolean = tokenProvider.validateToken(finalJwtToken, userDetails);
                                if (Boolean.TRUE.equals(aBoolean)) {
                                    UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken = new UsernamePasswordAuthenticationToken(userDetails, null, userDetails.getAuthorities());
                                    usernamePasswordAuthenticationToken.setDetails(new WebAuthenticationDetailsSource());
                                    SecurityContextHolder.getContext().setAuthentication(usernamePasswordAuthenticationToken);
                                }
                                String mail = userDetails.getUsername();
                                Collection<? extends GrantedAuthority> authorities = userDetails.getAuthorities();
                                return chain.filter(exchange).subscriberContext(ReactiveSecurityContextHolder
                                        .withAuthentication(
                                                new UsernamePasswordAuthenticationToken(mail, "", authorities)));
                            });
                }
            }
        }
        return chain.filter(exchange);
    }
}
