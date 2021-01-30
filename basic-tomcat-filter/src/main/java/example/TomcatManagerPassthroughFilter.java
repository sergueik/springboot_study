package example;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Enumeration;
import java.util.LinkedHashMap;
import java.util.List;

import javax.servlet.Filter;
import javax.servlet.FilterChain;
import javax.servlet.FilterConfig;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletResponse;

// filter allowing to bypass restrictions applied by tomcat in regards to redeploying a / webapp
// based on:
public final class TomcatManagerPassthroughFilter implements Filter {

	private List<Header> earlyHeaders = null;
	private List<Header> lateHeaders = null;
	private final boolean debug = false;

	@Override
	public void init(FilterConfig am) throws ServletException {
	}

	@Override
	public void doFilter(ServletRequest request, ServletResponse response,
			FilterChain chain) throws IOException, ServletException {
		final String page = request.toString();
		if (!(page.startsWith("manager") || page.equals("lbAccessStatus")
				|| page.contains("server-status"))) {
			chain.doFilter(request, response);
			return;
		}
	}

	@Override
	public void destroy() {
	}
}
