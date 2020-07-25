package example;

import java.io.*;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.Servlet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import example.Utils;

// https://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServlet.html

@SuppressWarnings("serial")
public class DemoServlet extends HttpServlet implements Servlet {
	private static final Logger logger = LogManager.getLogger(DemoServlet.class);
	private String message = "message";

	@Override
	public void init() throws ServletException {
		logger.debug("init()");
	}

	// inherited from class javax.servlet.GenericServlet
	// see also:
	// https://www.cs.tut.fi/lintula/manual/java/tutorial/servlets/lifecycle/service-threads.html
	@Override
	public void destroy() {
		logger.debug("destroy()");
	}

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// when no Log4j 2 configuration file found in CLASSPATH, log4j considers
		// itself not configured and switches to default configuration: logging the
		// errors to console only
		logger.debug("GET request=" + request.getContextPath() + " "
				+ request.getQueryString() + " " + Utils.getHeadersInfo(request));

		logger.info("GET request=" + request.getContextPath() + " "
				+ request.getQueryString());
		// @formatter:off
		String html = String.format("<html><head><title>Demo</title></head>" 
			+ "<body>You requested=[%s?%s]"				
			+ "<hr/>" 
			+ "%s " 
			+ "<hr/>" 
			+ "%s " 
			+ "</body></html>",
				request.getRequestURL(), request.getQueryString(), message,
				Utils.printHeadersInfo(Utils.getHeadersInfo(request)));
		// @formatter:on

		response.setContentType("text/html");
		response.getOutputStream().println(html);
	}

	@Override
	protected void doPost(final HttpServletRequest request,
			final HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
}

