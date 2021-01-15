package example;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;

import javax.servlet.Servlet;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

// https://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServlet.html

@SuppressWarnings("serial")
public class DemoServlet extends HttpServlet implements Servlet {
	private static final Logger logger = LogManager.getLogger(DemoServlet.class);
	private String message = null;

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
		logger.debug(request.getMethod().toUpperCase() + " request: "
				+ request.getContextPath() + " " + request.getQueryString());
		processRequest(request, response);

	}

	// https://stackoverflow.com/questions/8100634/get-the-post-request-body-from-httpservletrequest
	// note: a more compact code is possible with Java 8 / Tomcat 8.5+
	protected String doPayload(final HttpServletRequest request) {
		BufferedReader bufferedReader = null;
		StringBuffer stringBuffer = new StringBuffer();
		try {
			bufferedReader = request.getReader();
			char[] charBuffer = new char[128];
			int bytesRead;
			while ((bytesRead = bufferedReader.read(charBuffer)) != -1) {
				stringBuffer.append(charBuffer, 0, bytesRead);
			}
		} catch (IOException e) {
			// ignore
		} finally {
			if (bufferedReader != null) {
				try {
					bufferedReader.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}

		// NOTE: getReader() only returns the payload the first time is called:
		// subsequent calls produce null
		if (message == null) {
			message = stringBuffer.toString();
		}
		return message;
	}

	@Override
	protected void doPost(final HttpServletRequest request,
			final HttpServletResponse response) throws ServletException, IOException {

		logger.debug(request.getMethod().toUpperCase() + " request: "
				+ request.getContextPath() + " " + doPayload(request));
		processRequest(request, response);
	}

	protected void processRequest(final HttpServletRequest request,
			final HttpServletResponse response) throws ServletException, IOException {
		// @formatter:off
		message = doPayload(request);
		String properties = doProperties();
		String html = String.format(
			"<html><head><title>Demo</title></head>" 
			+ "<body>You requested=[%s?%s]\n"
			+ "<hr/>"
			+ "response: %s\n"
			+ "<hr/>"
			+ "properties: %s\n"
			+ "<hr/>"
			+ "headers %s\n"
			+ "</body></html>",
		// @formatter:on

		request.getRequestURL(), request.getQueryString(), message, properties,
				Utils.printHeadersInfo(Utils.getHeadersInfo(request)));

		// NOTE: when no Log4j2 configuration file found in CLASSPATH, log4j
		// considers
		// itself not configured and switches to default configuration: logging the
		// errors to console only

		response.setContentType("text/html");
		response.getOutputStream().println(html);

	}

	// Added method solely to execute property file loading exercise, needed for tcserver	
	private String doProperties() {
		String message = null;
		BufferedReader bufferedReader = null;
		StringBuffer stringBuffer = new StringBuffer();
		Properties properties = new Properties();
		InputStream input = null;
		// TODO: define on the page, pass through taglib
		String propertiesFile = "application.properties";
		String propertiesPath = "/opt/tomcat/conf";
		String propertyName = "application.setting";
		try {
			// TODO: prepend server root
			// input = new FileInputStream(propertiesFile);
			input = Thread.currentThread().getContextClassLoader()
					.getResourceAsStream(propertiesFile);
			if (input == null) {

				System.err.println("Failed to get properties file resource as stream: "
						+ propertiesFile);
				stringBuffer.append("Failed to get properties file resource as stream: "
						+ propertiesFile);
			} else {
				properties.load(input);
				System.err.println(
						propertyName + " = " + properties.getProperty(propertyName));
				stringBuffer.append(
						propertyName + " = " + properties.getProperty(propertyName));
			}
		} catch (IOException e) {
			stringBuffer.append("Exception: " + e.toString());
			System.err.println("Exception: " + e.toString());
		} catch (Exception e) {
			stringBuffer.append("Exception: " + e.toString());
			System.err.println("Exception: " + e.toString());
		} finally {
			if (input != null) {
				try {
					input.close();
				} catch (IOException e) {
					stringBuffer.append("Exception: " + e.toString());
					System.err.println("Exception: " + e.toString());
				}
			}
		}
		if (message == null) {
			message = stringBuffer.toString();
		}
		return message;
	}
}
