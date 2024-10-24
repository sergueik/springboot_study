package example;

import static example.Configuration.getErrorHandler;
import static example.Configuration.getObjectMapper;
import static example.Configuration.getUserService;
import static example.api.ApiUtils.splitQuery;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.List;
import java.util.Map;
import java.util.ArrayList;
import java.util.Arrays;

import com.sun.net.httpserver.BasicAuthenticator;
import com.sun.net.httpserver.HttpContext;
import com.sun.net.httpserver.HttpServer;

import example.api.user.RegistrationHandler;

@SuppressWarnings("restriction")
class Application {

	public static void main(String[] args) throws IOException {
		int serverPort = 8000;
		HttpServer server = HttpServer.create(new InetSocketAddress(serverPort), 0);

		RegistrationHandler registrationHandler = new RegistrationHandler(
				getUserService(), getObjectMapper(), getErrorHandler());
		// NO AUTH
		// can play role of health check
		server.createContext("/api/hello", (exchange -> {

			if ("GET".equals(exchange.getRequestMethod())) {
				Map<String, List<String>> params = splitQuery(
						exchange.getRequestURI().getRawQuery());
				String noNameText = "Anonymous";
				// see also
				// https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/List.html#of(E)
				String name = params.getOrDefault("name", new ArrayList<String>())
						.stream().findFirst().orElse(noNameText);
				String respText = String.format("Hello %s!", name);
				exchange.sendResponseHeaders(200, respText.getBytes().length);
				OutputStream output = exchange.getResponseBody();
				output.write(respText.getBytes());
				output.flush();
			} else {
				exchange.sendResponseHeaders(405, -1);// 405 Method Not Allowed
			}
			exchange.close();
		}));

		HttpContext context = server.createContext("/api/users/register",
				exchange -> {
					registrationHandler.handle(exchange);
				});
		context.setAuthenticator(new BasicAuthenticator("myrealm") {
			@Override
			public boolean checkCredentials(String user, String pwd) {
				System.err.println(String.format(
						"checking authentication: user: \"%s\" password:\"%s\"", user,
						pwd));
				return user.replaceAll("\n", "").equals("admin")
						&& pwd.replaceAll("\n", "").equals("admin");
			}
		});
		server.setExecutor(null); // creates a default executor
		server.start();
	}
}
