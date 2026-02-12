package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.IOException;
import java.net.ServerSocket;
import java.net.URI;
import java.net.URLDecoder;
import java.nio.charset.StandardCharsets;
import java.nio.file.Paths;
import java.util.Locale;
import java.util.Set;

import org.apache.commons.lang3.StringUtils;

// import org.apache.http.impl.bootstrap.HttpServer;
import org.apache.http.entity.ContentType;
import org.apache.http.protocol.HttpContext;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.CoreMatchers.notNullValue;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.io.File;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;

import example.DotGraphics;
import example.GraphViz;

class GraphvizHttpIntegrationTest {

	// treat DotGraphics as a black-box server
	static DotGraphics server;
	static int port;

	@BeforeAll
	static void startServer() throws Exception {
		port = getFreePort();
		// port = 18080; // or pick random free port
		server = new DotGraphics(port);
		server.start();
	}

	@AfterAll
	static void stopServer() {
		// Gracefully stop the DotGraphics server
		// Closing the ServerSocket forcibly unblocks the listener thread,
		// which will throw a SocketException ("Socket closed") in the thread.
		// This is expected and harmless: it only indicates the listener
		// noticed the shutdown. All requests have already completed.
		server.stop();
	}

	@DisplayName("returns 503 when Graphviz is not ready")
	@Test
	void test1() throws Exception {
		GraphViz.setReady(false);

		HttpRequest request = HttpRequest.newBuilder().uri(URI.create("http://localhost:" + port + "/"))
				.POST(HttpRequest.BodyPublishers.ofString("digraph { a->b }")).build();

		HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());

		assertThat(response.statusCode(), is(503));
		assertThat(response.body(), containsString("not ready"));
	}

	// @Disabled
	@DisplayName("returns 400 when payload is not a valid dot script")
	@Test
	void test2() throws Exception {
		GraphViz.setReady(true);

		HttpRequest request = HttpRequest.newBuilder().uri(URI.create("http://localhost:" + port + "/"))
				.POST(HttpRequest.BodyPublishers.ofString("nonsense")).build();

		HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());

		assertThat(response.statusCode(), is(400));
		assertThat(response.body(), containsString("not a valid dot content"));
	}

	@DisplayName("returns 501 when method is not POST ot HEAD")
	@Test
	void test3() throws Exception {
		GraphViz.setReady(true);

		HttpRequest request = HttpRequest.newBuilder().uri(URI.create("http://localhost:" + port + "/"))
				.PUT(HttpRequest.BodyPublishers.ofString("")).build();
		HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
		assertThat(response.statusCode(), is(501));
	}

	@DisplayName("returns 200 when payload is a valid dot script")
	@Test
	void test4() throws Exception {
		GraphViz.setReady(true);

		HttpRequest request = HttpRequest.newBuilder().uri(URI.create("http://localhost:" + port + "/"))
				.POST(HttpRequest.BodyPublishers.ofString("graph { a -- b }")).build();
		HttpResponse<byte[]> response = HttpClient.newHttpClient().send(request,
				HttpResponse.BodyHandlers.ofByteArray());
		byte[] body = response.body();
		assertThat(response.statusCode(), is(200));
		try (ByteArrayInputStream in = new ByteArrayInputStream(body)) {
			BufferedImage img = ImageIO.read(in);
			assertThat(img, notNullValue());
			assertThat(img.getType(), is(BufferedImage.TYPE_4BYTE_ABGR));
			System.out.printf("PNG image data, %d x %d%n", img.getWidth(), img.getHeight());
		}
	}

	private static int getFreePort() throws IOException {
		try (ServerSocket socket = new ServerSocket(0)) {
			socket.setReuseAddress(true);
			return socket.getLocalPort();
		}
	}
}
