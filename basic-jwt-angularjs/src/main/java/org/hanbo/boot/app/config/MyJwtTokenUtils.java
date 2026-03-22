package org.hanbo.boot.app.config;

import java.io.Serializable;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Function;

import org.hanbo.boot.app.models.LoginUser;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.thymeleaf.util.StringUtils;

import com.fasterxml.jackson.databind.ObjectMapper;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;

@SuppressWarnings("serial")
@Component
public class MyJwtTokenUtils implements Serializable {

// 	private static final long serialVersionUID = -2550185165626007488L;

	public static final long JWT_TOKEN_VALIDITY = 15 * 60; // 15 minutes

	@Value("${jwt.secret}")
	private String secret;

	public LoginUser getUserInfoFromToken(String token) {
		LoginUser retVal = null;
		String userInfoStr = getUserInfoStringFromToken(token);
		if (!StringUtils.isEmpty(userInfoStr)) {
			ObjectMapper mapper = new ObjectMapper();
			try {
				retVal = mapper.readValue(userInfoStr, LoginUser.class);
			} catch (Exception ex) {
				System.out.println("Exception occurred. " + ex.getMessage());
				ex.printStackTrace();
				retVal = null;
			}
		}

		return retVal;
	}

	public String getUserInfoStringFromToken(String token) {
		String retVal = null;
		if (!StringUtils.isEmpty(token)) {
			retVal = getClaimFromToken(token, Claims::getSubject);
		}

		return retVal;
	}

	public Date getExpirationDateFromToken(String token) {
		Date retVal = null;
		if (!StringUtils.isEmpty(token)) {
			retVal = getClaimFromToken(token, Claims::getExpiration);
		}

		return retVal;
	}

	public <T extends Object> T getClaimFromToken(String token, Function<Claims, T> claimsResolver) {
		if (!StringUtils.isEmpty(token)) {
			Claims claims = getAllClaimsFromToken(token);
			return claimsResolver.apply(claims);
		} else {
			return null;
		}
	}

	private Claims getAllClaimsFromToken(String token) {
		if (!StringUtils.isEmpty(token)) {
			if (!StringUtils.isEmpty(secret)) {
				return Jwts.parser().setSigningKey(secret).parseClaimsJws(token).getBody();
			} else {
				System.out.println("Secret key is null or empty, unable to decode claims from token.");
				return null;
			}
		} else {
			return null;
		}
	}

	private Boolean isTokenExpired(String token) {
		if (!StringUtils.isEmpty(token)) {
			Date expiration = getExpirationDateFromToken(token);
			if (expiration == null) {
				System.out.println("Invalid expiration data. Invalid token detected.");
				return false;
			}

			return expiration.before(new Date());
		}

		return false;
	}

	public String generateToken(LoginUser userInfo, Map<String, Object> claims) {
		String userInfoStr = "";
		String retVal = null;

		if (claims == null) {
			System.out.println("Claims object is null or empty, cannot createsecurity token.");
			return retVal;
		}

		if (userInfo == null) {
			System.out.println("userInfo object is null or empty, cannot createsecurity token.");
			return retVal;
		}

		try {
			ObjectMapper mapper = new ObjectMapper();
			userInfoStr = mapper.writeValueAsString(userInfo);

			retVal = doGenerateToken(claims, userInfoStr);
		} catch (Exception ex) {
			System.out.println("Exception occurred. " + ex.getMessage());
			ex.printStackTrace();
			retVal = null;
		}

		return retVal;
	}

	public String generateToken(LoginUser userDetails) {
		Map<String, Object> emptyClaims = new HashMap<String, Object>();
		return generateToken(userDetails, emptyClaims);
	}

	private String doGenerateToken(Map<String, Object> claims, String subject) {
		String retVal = null;

		if (StringUtils.isEmpty(secret)) {
			System.out.println("Invalid secret key for token encryption.");
			return retVal;
		}

		if (claims == null) {
			System.out.println("Invalid token claims object.");
			return retVal;
		}

		if (StringUtils.isEmpty(subject)) {
			System.out.println("Invalid subject value for the security token.");
			return retVal;
		}

		return Jwts.builder().setClaims(claims).setSubject(subject).setIssuedAt(new Date(System.currentTimeMillis()))
				.setExpiration(new Date(System.currentTimeMillis() + (JWT_TOKEN_VALIDITY * 1000)))
				.signWith(SignatureAlgorithm.HS512, secret).compact();
	}

	public Boolean validateToken(String token, LoginUser userDetails) {
		if (!StringUtils.isEmpty(token)) {
			LoginUser userInfo = getUserInfoFromToken(token);

			if (userInfo != null) {
				if (userDetails != null) {
					String actualUserId = userInfo.getUserId();
					if (!StringUtils.isEmpty(actualUserId) && actualUserId.equalsIgnoreCase(userDetails.getUserId())) {
						if (userDetails.isActive()) {
							return !isTokenExpired(token);
						} else {
							System.out.println(String.format("User with id [%s] is not active. Invalid token.",
									userInfo.getUserId()));
							return false;
						}
					} else {
						System.out.println("User in the token has a different user id than expected. Invalid token.");
						return false;
					}
				} else {
					System.out.println("Expected user details object is invalid. Unable to verify token validity.");
					return false;
				}
			} else {
				System.out.println("Decrypted user details object is invalid. Invalid token.");
				return false;
			}
		} else {
			System.out.println("Invalid token string detected. Invalid token.");
			return false;
		}
	}
}