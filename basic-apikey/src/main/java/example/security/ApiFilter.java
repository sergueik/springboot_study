package example.security;

import org.springframework.security.web.authentication.preauth.AbstractPreAuthenticatedProcessingFilter;
import javax.servlet.http.HttpServletRequest;

public class ApiFilter extends AbstractPreAuthenticatedProcessingFilter {

	private String headerName;

	public ApiFilter(final String data) {
		headerName = data;
	}

	@Override
	protected Object getPreAuthenticatedPrincipal(HttpServletRequest request) {
		return request.getHeader(headerName);
	}

	@Override
	protected Object getPreAuthenticatedCredentials(HttpServletRequest request) {
		return null;
	}
}
