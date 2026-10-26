package example;

import com.sun.net.httpserver.HttpExchange;
import java.net.HttpURLConnection;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpServer;
import java.net.InetSocketAddress;

import java.io.IOException;
import java.nio.file.Path;
import java.nio.file.Paths;

import java.nio.file.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class App {
	private static boolean debug = false;

	public static void main(String[] args) throws Exception {
		Map<String, String> cli = parseArgs(args);

		String dir = "";
		String root = Paths.get(System.getProperty("java.io.tmpdir"), "repo").toString();
		String host = "0.0.0.0"; // prepare to Kubernetes binding
		int port = 8081;

		if (cli.containsKey("debug"))
			debug = Boolean.parseBoolean(cli.get("debug"));

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("root"))
			root = cli.get("root");
		if (cli.containsKey("dir"))
			dir = cli.get("dir");
		if (cli.containsKey("host"))
			host = cli.get("host");
		if (cli.containsKey("port"))
			port = Integer.parseInt(cli.get("port"));

		final String documentRoot = root;
		String directory = dir.trim();
		if (directory.isEmpty() || directory.equals("/")) {
			directory = "/";
		} else {
			if (!directory.startsWith("/")) {
				directory = "/" + directory;
			}
			while (directory.endsWith("/")) {
				directory = directory.substring(0, directory.length() - 1);
			}
		}
		final String contextPath = directory;
		final int length = contextPath.equals("/") ? 1 : contextPath.length();
		Path path = Paths.get(documentRoot);

		// bind to host argument
		HttpServer server = HttpServer.create(new InetSocketAddress(host, port), 0);
		server.createContext(directory, (HttpExchange exchange) -> {
			Path file = null;
			try {
				String requestPath = exchange.getRequestURI().getPath();
				String relativePath = requestPath.substring(length);
				while (relativePath.startsWith("/")) {
					relativePath = relativePath.substring(1);
				}
				file = secureResolve(documentRoot, relativePath);
			} catch (IOException | SecurityException e) {
				System.err.println(String.format("Exception %s from ", e.toString(), exchange.getRemoteAddress()));
				exchange.sendResponseHeaders(HttpURLConnection.HTTP_NOT_FOUND, -1);
				exchange.close();
				return;
			}
			// NOTE: do not provide directory listing
			if (Files.isRegularFile(file)) {
				byte[] data = Files.readAllBytes(file);
				exchange.sendResponseHeaders(HttpURLConnection.HTTP_OK, data.length);
				exchange.getResponseBody().write(data);
			} else {
				exchange.sendResponseHeaders(HttpURLConnection.HTTP_NOT_FOUND, -1);
			}
			exchange.close();
		});

		server.start();
		System.err.println(String.format("Repo server on %s:%d with document root: %s directory %s", host, port,
				documentRoot, contextPath));
	}

	private static Path secureResolve(final String baseDir, final String relativePath) throws IOException {
		Path base = Paths.get(baseDir).toRealPath();

		Path target = base.resolve(relativePath).normalize();

		if (!target.startsWith(base)) {
			throw new SecurityException(String.format("Path traversal attempt detected: %s", relativePath));
		}

		Path realTarget = target.toRealPath();

		if (!realTarget.startsWith(base)) {
			throw new SecurityException(String.format("Resolved path escapes repository root:  %s", relativePath));
		}

		return realTarget;
	}

	private static Map<String, String> parseArgs(String[] args) {
		if (Arrays.asList(args).contains("debug"))
			System.err.println("Processing: " + Arrays.asList(args));
		Map<String, String> map = new HashMap<>();
		for (int i = 0; i < args.length - 1; i++) {
			if (args[i].startsWith("-")) {
				map.put(args[i].substring(1), args[i + 1]);
				i++;
			}
		}
		return map;
	}
}