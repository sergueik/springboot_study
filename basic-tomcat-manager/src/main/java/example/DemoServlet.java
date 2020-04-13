package example;

import java.io.*;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.Servlet;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

// https://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServlet.html
// 
@SuppressWarnings("serial")
public class DemoServlet extends HttpServlet implements Servlet {
	private static final Logger logger = LogManager.getLogger(DemoServlet.class);
	private String message;
	private final String textFile = "readme.textile";

	@Override
	public void init() throws ServletException {
		message = "message";
		logger.info("init()");
	}

	// inherited from class javax.servlet.GenericServlet
	// see also:
	// https://www.cs.tut.fi/lintula/manual/java/tutorial/servlets/lifecycle/service-threads.html
	@Override
	public void destroy() {
		logger.error("destroy()");
	}

	@Override
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		// when no Log4j 2 configuration file is found in CLASSPATH, log4j consider
		// itself not properly configured. with default configuration is logging
		// only
		// errors to the console
		logger.error("responding to GET request=" + request);
		logger.info("responding to GET request=" + request);

		// message = FileUtils.readFileToString(new File(textFile));
		String html = String.format(
				"<html><head><title>Demo</title></head>" + "<body>You requested=[%s?%s]"
						+ "<hr/>" + "%s</body></html>",
				request.getRequestURL(), request.getQueryString(), message);

		response.setContentType("text/html");
		response.getOutputStream().println(html);
	}

	@Override
	protected void doPost(final HttpServletRequest request,
			final HttpServletResponse response) throws ServletException, IOException {
		doGet(request, response);
	}
}
