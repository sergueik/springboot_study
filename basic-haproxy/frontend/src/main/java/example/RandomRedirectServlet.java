package example;

import java.io.*;
import java.util.Random;
import java.util.Vector;

import javax.servlet.*;
import javax.servlet.http.*;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

// origin: http://www.java2s.com/Tutorial/Java/0400__Servlet/RedirectServletbySettingResponseLocation.htm
@SuppressWarnings("serial")
public class RandomRedirectServlet extends HttpServlet {

	private static final Logger logger = LogManager.getLogger(RandomRedirectServlet.class);

	Vector<String> apps = new Vector<String>();
	Random random = new Random();

	private static String redirectHost = getPropertyEnv("REDIRECT_HOST","localhost");
	private static String redirectPort = getPropertyEnv("REDIRECT_PORT","8080");
	private static String baseUrl = "http://" + redirectHost + ":" + redirectPort;

	public void init() throws ServletException {
		apps.addElement("/app1/index.jsp"); // admin
		apps.addElement("/app2/index.jsp"); // about
		apps.addElement("/app3/index.jsp"); // products
	}

	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws ServletException, IOException {
		response.setContentType("text/html");
		PrintWriter out = response.getWriter();

		String app = (String) apps
				.elementAt(Math.abs(random.nextInt()) % apps.size());

		response.setStatus(response.SC_MOVED_TEMPORARILY);
		logger.info("Redirect to {}", baseUrl + app);
		response.setHeader("Location", baseUrl + app);
	}
	// origin:
	// https://github.com/TsvetomirSlavov/wdci/blob/master/code/src/main/java/com/seleniumsimplified/webdriver/manager/EnvironmentPropertyReader.java
	public static String getPropertyEnv(String name, String defaultValue) {
		String value = System.getProperty(name);
		if (value == null || value.length() == 0) {
			value = System.getenv(name);
			if (value == null || value.length() == 0) {
				value = defaultValue;
			}
		}
		return value;
	}
}
