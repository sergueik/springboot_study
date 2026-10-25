package example;

import com.sun.net.httpserver.HttpServer;
import java.net.InetSocketAddress;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;
import java.io.IOException;
import java.io.OutputStream;

@SuppressWarnings("restriction")
public class App {
	private static boolean debug = false;

	public static void main(String[] args) throws Exception {
		Map<String, String> cli = parseArgs(args);

		boolean live = true;
		int port = 8080;
		boolean ready = true;

		if (cli.containsKey("debug"))
			debug = Boolean.parseBoolean(cli.get("debug"));

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("ready"))
			ready = "up".equalsIgnoreCase(cli.get("ready"));
		if (cli.containsKey("live"))
			live = "up".equalsIgnoreCase(cli.get("live"));
		if (cli.containsKey("port"))
			port = Integer.parseInt(cli.get("port"));

		HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);

		// hard code the 3 routes
		StatusHandler readyHandler = new StatusHandler();
		readyHandler.setStatus(ready);
		server.createContext("/", new HomeHandler());
		server.createContext("/api/live", new StatusHandler(live));
		server.createContext("/api/ready", readyHandler /* new StatusHandler(ready) */);

		server.setExecutor(null); // Creates a default executor
		System.err.println("Server started on port " + port);
		server.start();
	}

	// Route 1: Home page - rejecting everything else
	static class HomeHandler implements HttpHandler {
		@Override
		public void handle(HttpExchange exchange) throws IOException {
			String path = exchange.getRequestURI().getPath();
			// NOTE: he package com.sun.net.httpserver (part of the jdk.httpserver module) does not contain a dedicated HttpStatus
			if (!"/".equals(path)) {
				sendResponse(exchange, "{\"status\": \"error\", \"error\": \"Not Found\"}", 404);
				return;
			}
			sendResponse(exchange, "Welcome!", 200);
		}
	}

	// Route 3: Liveness/Readiness endpoint
	static class StatusHandler implements HttpHandler {
		private boolean status = true;

		public StatusHandler() {
		}

		public StatusHandler(boolean status) {
			super();
			this.status = status;
		}

		public void setStatus(boolean value) {
			status = value;
		}

		@Override
		public void handle(@SuppressWarnings("restriction") HttpExchange exchange) throws IOException {
			String path = exchange.getRequestURI().getPath();
			if (!"/api/live".equals(path) && !"/api/ready".equals(path)) {
				sendResponse(exchange, "{\"status\": \"error\", \"error\": \"Not Found\"}", 404);
				return;
			}
			sendResponse(exchange, String.format("{\"status\": \"%s\"}", status ? "UP" : "DOWN"), status ? 200 : 503);
		}
	}

	private static void sendResponse(HttpExchange exchange, String response, int statusCode) throws IOException {
		exchange.sendResponseHeaders(statusCode, response.length());
		try (OutputStream os = exchange.getResponseBody()) {
			os.write(response.getBytes());
		}
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