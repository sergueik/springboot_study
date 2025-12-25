package com.bookportal.api.auth;

import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.ReactiveAuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.Collection;

@Component
@RequiredArgsConstructor
public class JwtAuthenticationManager implements ReactiveAuthenticationManager {
    private final MyReactiveUserDetailsService jwtUserDetailsService;
    private final JwtTokenProvider jwtTokenProvider;

    @Override
    public Mono<Authentication> authenticate(Authentication authentication) {
        return Mono.just(authentication)
                .flatMap(jwtTokenProvider::validateAndGetClaimsFromToken)
                .onErrorStop()
                .flatMap(claims -> jwtUserDetailsService.findByUsername(claims.getSubject())
                        .map(userDetails -> {
                            String mail = claims.getSubject();
                            Collection<? extends GrantedAuthority> authorities = userDetails.getAuthorities();
                            return new UsernamePasswordAuthenticationToken(mail, "", authorities);
                        }));
    }
}
