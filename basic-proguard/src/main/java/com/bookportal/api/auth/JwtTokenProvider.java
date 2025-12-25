package com.bookportal.api.auth;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.entity.Role;
import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.DecodingException;
import lombok.RequiredArgsConstructor;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Mono;

import java.util.ArrayList;
import java.util.Date;
import java.util.function.Function;
import java.util.stream.Collectors;

@Component
@RequiredArgsConstructor
public class JwtTokenProvider {
    private final EnvironmentVariables env;
    private static final long DAY = 1;
    public static final long JWT_TOKEN_VALIDITY = 30 * DAY * (24 * 60 * 60 * 1000);
    private static final String ROLE_SEPARATOR = ":";
    @Value("${jwt.secret}")
    private String secret;

    public String generateToken(String mail, ArrayList<Role> roles) {
        String collect = roles.stream().map(Role::getName).collect(Collectors.joining(ROLE_SEPARATOR));
        return Jwts.builder()
                .claim("roles", collect)
                .setSubject(mail)
                .setIssuedAt(new Date(System.currentTimeMillis()))
                .setExpiration(new Date(System.currentTimeMillis() + JWT_TOKEN_VALIDITY))
                .signWith(SignatureAlgorithm.HS256, secret)
                .compact();
    }

    public Mono<Claims> validateAndGetClaimsFromToken(Authentication authentication) {
        String token = authentication.getCredentials().toString();
        final String username = getUsernameFromToken(token);
        if (!username.equals(authentication.getName())) {
            throw new IllegalArgumentException(env.tokenUserNotValid());
        }
        if (token.contains("Bearer")) {
            token = token.replace("Bearer ", "");
        } else {
            throw new DecodingException("");
        }
        return Mono.just(getClaimsFromToken(token));
    }

    public Claims getClaimsFromToken(String token) {
        return Jwts.parser().setSigningKey(secret).parseClaimsJws(token).getBody();
    }

    public String getUsernameFromToken(String token) {
        return getClaimFromToken(token, Claims::getSubject);
    }

    private <T> T getClaimFromToken(String token, Function<Claims, T> claimsResolver) {
        final Claims claims = getClaimsFromToken(token);
        return claimsResolver.apply(claims);
    }

    public Date getExpirationDateFromToken(String token) {
        return getClaimFromToken(token, Claims::getExpiration);
    }

    private Boolean isTokenExpired(String token) {
        final Date expiration = getExpirationDateFromToken(token);
        return expiration.before(new Date());
    }

    public Boolean validateToken(String token, UserDetails userDetails) {
        final String username = getUsernameFromToken(token);
        return (username.equals(userDetails.getUsername()) && !isTokenExpired(token));
    }
}
