package example;

import java.net.HttpURLConnection;
import java.net.InetSocketAddress;

import com.sun.net.httpserver.HttpServer;
import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpHandler;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.io.OutputStream;
import java.io.IOException;

import java.nio.channels.FileChannel;
import java.nio.channels.FileLock;
import java.nio.channels.OverlappingFileLockException;

import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.util.Collections;

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
		// will return HTTP Status-Code 503: Service Unavailable if not ready
		if (cli.containsKey("live"))
			live = "up".equalsIgnoreCase(cli.get("live"));
		if (cli.containsKey("port"))
			port = Integer.parseInt(cli.get("port"));

		HttpServer server = HttpServer.create(new InetSocketAddress(port), 0);

		// NOTE: serve the 3 hard coded routes only
		// Return HTTP Status-Code 404: Not Found for everything else
		StatusHandler readyHandler = new StatusHandler();
		readyHandler.setStatus(ready);
		server.createContext("/", new HomeHandler());
		server.createContext("/api/live", new StatusHandler(live));
		server.createContext("/api/ready", readyHandler);
		server.createContext("/api/failure/file", new HttpHandler() {

			ExclusiveFileOpener exclusiveFileOpener = new ExclusiveFileOpener();
			FileLock lock = null;

			@Override
			public void handle(HttpExchange exchange) throws IOException {

				String query = exchange.getRequestURI().getQuery();
				Map<String, String> params = parseQuery(query);

				// Example: access a parameter named "name"
				boolean lockResource = Boolean.parseBoolean(params.getOrDefault("lock", "false"));
				String resourcePath = params.getOrDefault("path", "path/to/your/file.txt");
				try {
					if (lockResource) {
						lock = exclusiveFileOpener.openExclusiveFile(resourcePath);
						System.err.println("Exclusive lock acquired successfully.");
						// Keep lock indefinitely...
					} else {
						// Release and close when done - will throw NPE
						lock.release();
						lock.channel().close();
					}
				} catch (OverlappingFileLockException e) {
					System.err.println("File is already locked within this Java machine.");
				} catch (IllegalStateException | IOException e) {
					System.err.println("Error: " + e.getMessage());
				}
			}

			private Map<String, String> parseQuery(String query) {
				if (query == null || query.isEmpty()) {
					return Collections.emptyMap();
				}
				String key = null;
				String value = null;
				Map<String, String> result = new HashMap<>();
				String[] tokens = query.split("&");

				for (String token : tokens) {
					int idx = token.indexOf("=");
					if (idx > 0) {
						key = URLDecoder.decode(token.substring(0, idx));
						value = URLDecoder.decode(token.substring(idx + 1));
						result.put(key, value);
					} else if (!token.isEmpty()) {
						key = URLDecoder.decode(token);
						result.put(key, "");
					}
				}

				return result;
			}

		});
		server.createContext("/api/failure/oom", new HttpHandler() {

			@Override
			public void handle(HttpExchange exchange) throws IOException {
				List<byte[]> memoryBurner = new ArrayList<>();
				long delay = 1000;
				int chunkSize = 2 * 1024 * 1024;
				System.err.println("Bleeding memory...");

				while (true) {
					// Keep adding 2MB chunks to the list
					memoryBurner.add(new byte[chunkSize]);
					try {
						Thread.sleep(delay);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}
				// throws java.lang.OutOfMemoryError
				// Java heap space

			}
		});

		server.setExecutor(null); // Creates a default executor
		System.err.println("Server started on port " + port);
		server.start();
	}

	// Route 1: Home page - rejecting everything else
	static class HomeHandler implements HttpHandler {
		@Override
		public void handle(HttpExchange exchange) throws IOException {
			String path = exchange.getRequestURI().getPath();
			// NOTE: he package com.sun.net.httpserver (part of the jdk.httpserver module)
			// does not contain a dedicated HttpStatus
			if (!"/".equals(path)) {
				sendResponse(exchange, "{\"status\": \"error\", \"error\": \"Not Found\"}",
						HttpURLConnection.HTTP_NOT_FOUND);
				return;
			}
			sendResponse(exchange, "Welcome!", HttpURLConnection.HTTP_OK);
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
		public void handle(HttpExchange exchange) throws IOException {
			String path = exchange.getRequestURI().getPath();
			if (!"/api/live".equals(path) && !"/api/ready".equals(path)) {
				sendResponse(exchange, "{\"status\": \"error\", \"error\": \"Not Found\"}",
						HttpURLConnection.HTTP_NOT_FOUND);
				return;
			}
			sendResponse(exchange, String.format("{\"status\": \"%s\"}", status ? "UP" : "DOWN"),
					status ? HttpURLConnection.HTTP_OK : HttpURLConnection.HTTP_UNAVAILABLE);
		}

	}
	// locking a file for exclusive access

	private static class ExclusiveFileOpener {

		public static FileLock openExclusiveFile(String filePath) throws IOException, IllegalStateException {
			File file = new File(filePath);

			// Open the file with read/write access
			RandomAccessFile raf = new RandomAccessFile(file, "rw");
			FileChannel channel = raf.getChannel();

			// Try to acquire an exclusive lock without blocking
			FileLock lock = channel.tryLock();

			if (lock == null) {
				channel.close();
				raf.close();
				throw new IllegalStateException("File is already locked by another process: " + filePath);
			}

			return lock;
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