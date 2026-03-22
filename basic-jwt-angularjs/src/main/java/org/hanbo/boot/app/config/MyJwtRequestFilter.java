package org.hanbo.boot.app.config;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.servlet.FilterChain;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.hanbo.boot.app.models.LoginUser;
import org.hanbo.boot.app.models.UserRole;
import org.hanbo.boot.app.security.MyJwtUserCheckService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.authentication.WebAuthenticationDetailsSource;
import org.springframework.stereotype.Component;
import org.springframework.util.StringUtils;
import org.springframework.web.filter.OncePerRequestFilter;

import io.jsonwebtoken.ExpiredJwtException;

@Component
public class MyJwtRequestFilter extends OncePerRequestFilter {
	private final String _authorizationKey = "authorization";
	private final String _bearerTokenPrefix = "bearer ";

	@Autowired
	private MyJwtUserCheckService jwtUserCheckService;

	@Autowired
	private MyJwtTokenUtils jwtTokenUtils;

	@Override
	protected void doFilterInternal(HttpServletRequest request, HttpServletResponse response, FilterChain chain)
			throws ServletException, IOException {
		System.out.println("--------------------------------");
		System.out.println(request.getRequestURL().toString());

		LoginUser tokenUserInfo = null;
		String jwtToken = getJwtTokenFromHeader(request);

		if (!StringUtils.isEmpty(jwtToken)) {
			tokenUserInfo = extractJwtUserInfoFromToken(jwtToken);
			if (tokenUserInfo != null) {
				SecurityContextHolder.getContext().setAuthentication(null);
				if (!StringUtils.isEmpty(tokenUserInfo.getUserId())) {
					LoginUser userDetails = this.jwtUserCheckService.getUserDetails(tokenUserInfo.getUserId());
					if (userDetails != null) {
						if (jwtTokenUtils.validateToken(jwtToken, userDetails)) {
							List<GrantedAuthority> allAuths = convertUserRolesToGrantedAuthorities(
									userDetails.getAllUserRoles());
							if (allAuths != null && allAuths.size() > 0) {
								UsernamePasswordAuthenticationToken usernamePasswordAuthenticationToken = new UsernamePasswordAuthenticationToken(
										userDetails, null, allAuths);
								usernamePasswordAuthenticationToken
										.setDetails(new WebAuthenticationDetailsSource().buildDetails(request));
								SecurityContextHolder.getContext()
										.setAuthentication(usernamePasswordAuthenticationToken);
							} else {
								System.out.println("User has no roles associated with.");
								SecurityContextHolder.getContext().setAuthentication(null);
							}
						} else {
							System.out.println("User credential cannot be validated.");
							SecurityContextHolder.getContext().setAuthentication(null);
						}
					} else {
						System.out.println("No valid user credential available.");
						SecurityContextHolder.getContext().setAuthentication(null);
					}
				} else {
					System.out.println("Invalid user info detected. Authentication failed.");
					SecurityContextHolder.getContext().setAuthentication(null);
				}
			} else {
				System.out.println("Unable to get JWT Token");
				SecurityContextHolder.getContext().setAuthentication(null);
			}
		} else {
			System.out.println("JWT Token does not begin with Bearer String");
			SecurityContextHolder.getContext().setAuthentication(null);
		}

		System.out.println("Try normal filtering");
		chain.doFilter(request, response);
		System.out.println("--------------------------------");
	}

	private String getJwtTokenFromHeader(HttpServletRequest request) {
		String retVal = "";
		if (request != null) {
			final String requestTokenHeader = request.getHeader(_authorizationKey);
			System.out.println("Found Auth Key: [" + requestTokenHeader + "]");
			if (!StringUtils.isEmpty(requestTokenHeader) && requestTokenHeader.startsWith(_bearerTokenPrefix)) {
				retVal = requestTokenHeader.substring(_bearerTokenPrefix.length());
			}
		}

		return retVal;
	}

	private LoginUser extractJwtUserInfoFromToken(String tokenStrVal) {
		LoginUser retVal = null;
		if (!StringUtils.isEmpty(tokenStrVal)) {
			try {
				retVal = jwtTokenUtils.getUserInfoFromToken(tokenStrVal);
			} catch (IllegalArgumentException ex) {
				System.out.println("Unable to get JWT Token via token string decryption.");
				retVal = null;
			} catch (ExpiredJwtException ex) {
				System.out.println("JWT Token has expired");
				retVal = null;
			}
		}

		return retVal;
	}

	private List<GrantedAuthority> convertUserRolesToGrantedAuthorities(List<UserRole> allUserRoles) {
		List<GrantedAuthority> retVal = new ArrayList<GrantedAuthority>();
		if (allUserRoles != null && allUserRoles.size() > 0) {
			for (UserRole role : allUserRoles) {
				if (role != null) {
					if (!StringUtils.isEmpty(role.getRoleId())) {
						retVal.add(new SimpleGrantedAuthority(role.getRoleId()));
					}
				}
			}
		}

		return retVal;
	}
}