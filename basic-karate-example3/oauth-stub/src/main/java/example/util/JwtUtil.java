package example.util;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;

import java.security.Key;
import java.util.List;

import javax.crypto.SecretKey;

@Component
public class JwtUtil {

	private final SecretKey key;

	// 🔑 STUB signing secret key
	// aligned with TokenService
	public JwtUtil(@Value("${example.base64Secret}") String base64Secret) {
		byte[] keyBytes = Decoders.BASE64.decode(base64Secret);
		this.key = Keys.hmacShaKeyFor(keyBytes);
	}

	private Claims parseClaims(String token) {
		return Jwts.parserBuilder().setSigningKey(key).build().parseClaimsJws(token).getBody();
	}

	public boolean validate(String token) {
		try {
			parseClaims(token);
			return true;
		} catch (Exception e) {
			return false;
		}
	}

	public String getUsername(String token) {
		return parseClaims(token).getSubject();
	}

	@SuppressWarnings("unchecked")
	public List<String> getRoles(String token) {
		Object roleObj = parseClaims(token).get("role");

		List<String> roles;

		if (roleObj instanceof String) {
			roles = List.of("ROLE_" + roleObj);
		} else if (roleObj instanceof List) {
			roles = (List<String>) roleObj;
		} else {
			roles = List.of();
		}

		return roles;
	}
}