package example.service;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.util.Date;

@Service
public class AuthService {

	@Value("${server.port}")
	private int port;
	
	// 🔑 STUB signing key
	@Value("${example.key}")
	private String key;

	public String generateToken(String username) {

		long now = System.currentTimeMillis();
		return Jwts.builder().setSubject(username) // "sub" claim
				.claim("role", "USER") // stubbed authorization
				.setIssuer(String.format("http://localhost:%d", port)) // identity provider
				.setIssuedAt(new Date(now)).setExpiration(new Date(now + 3600_000)) // 1 hour
				.signWith(SignatureAlgorithm.HS256, key).compact();
	}
}
