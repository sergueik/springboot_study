package example;

import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpHandler;
import com.sun.net.httpserver.HttpExchange;

import java.io.*;
import java.net.InetSocketAddress;
import java.nio.file.*;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class App {
	private static boolean debug = false;

	public static void main(String[] args) throws Exception {
		Map<String, String> cli = parseArgs(args);

		String root = Paths.get(System.getProperty("java.io.tmpdir"), "repo").toString();
		Integer port = 8081;

		if (cli.containsKey("debug"))
			debug = Boolean.parseBoolean(cli.get("debug"));

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("root"))
			root = cli.get("root");
		if (cli.containsKey("port"))
			port = Integer.parseInt(cli.get("port"));

		Path path = Paths.get(root);

		HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);
		server.createContext("/", exchange -> {
			Path file = path.resolve(exchange.getRequestURI().getPath().substring(1));
			if (Files.exists(file) && !Files.isDirectory(file)) {
				byte[] data = Files.readAllBytes(file);
				exchange.sendResponseHeaders(200, data.length);
				exchange.getResponseBody().write(data);
			} else {
				exchange.sendResponseHeaders(404, -1);
			}
			exchange.close();
		});

		server.start();
		System.out.println(String.format("Repo server on :%d", port));
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
