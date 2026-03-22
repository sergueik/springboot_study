package org.hanbo.boot.app.config;

import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.stereotype.Component;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

@Component
public class MyAccessDeniedHandler implements AccessDeniedHandler {
	@Override
	public void handle(HttpServletRequest req, HttpServletResponse resp, AccessDeniedException ex)
			throws IOException, ServletException {
		Authentication auth = SecurityContextHolder.getContext().getAuthentication();

		if (auth != null) {
			System.out.println(
					"User '" + auth.getName() + "' attempted to access the protected URL: " + req.getRequestURI());
			resp.sendError(HttpServletResponse.SC_FORBIDDEN, "Access Forbidden");
		} else {
			resp.sendError(HttpServletResponse.SC_UNAUTHORIZED, "Unauthorized");
		}
	}
}