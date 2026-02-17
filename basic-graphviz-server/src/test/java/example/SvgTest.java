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
import java.net.http.HttpRequest.BodyPublisher;
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

class SvgTest {

	// treat DotGraphics as a black-box server
	static DotGraphics server;
	static int port;
	static String url = null;
	private BodyPublisher body = HttpRequest.BodyPublishers.ofString("graph { a -- b }");

	@BeforeAll
	static void startServer() throws Exception {
		port = getFreePort();
		// port = 18080; // or pick random free port
		server = new DotGraphics(port);
		server.start();
		url = "http://localhost:" + port + "/";
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

	// @Disabled
	@DisplayName("returns 400 when ?format argument is not svg or png")
	@Test
	void test2() throws Exception {
		GraphViz.setReady(true);
		HttpRequest request = HttpRequest.newBuilder().uri(URI.create(url + "?format=unknown")).POST(body).build();
		HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
		assertThat(response.statusCode(), is(400));
	}

	// @Disabled
	@DisplayName("returns 400 when format is passed through uri but not svg or png")
	@Test
	void test3() throws Exception {
		GraphViz.setReady(true);
		HttpRequest request = HttpRequest.newBuilder().uri(URI.create(url + "unknownformat")).POST(body).build();
		HttpResponse<String> response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofString());
		assertThat(response.statusCode(), is(400));
	}

	// @Disabled
	@DisplayName("returns 200 and SVG payload when format=svg is requested")
	@Test
	void test4() throws Exception {
		GraphViz.setReady(true);

		HttpRequest request = HttpRequest.newBuilder().uri(URI.create(url + "?format=svg")).POST(body).build();

		HttpResponse<byte[]> response = HttpClient.newHttpClient().send(request,
				HttpResponse.BodyHandlers.ofByteArray());

		// status
		assertThat(response.statusCode(), is(200));

		// content type (response header, not request!)
		assertThat(response.headers().firstValue("Content-Type").orElse(""), containsString(ContentType.APPLICATION_SVG_XML.getMimeType()));

		byte[] bytes = response.body();
		assertThat(bytes, notNullValue());
		assertThat(bytes.length, greaterThan(0));

		// peek into SVG
		String svg = new String(bytes, StandardCharsets.UTF_8);

		assertThat(svg, containsString("<svg"));
		assertThat(svg, containsString("a"));
		assertThat(svg, containsString("b"));

		System.out.println("SVG length = " + svg.length());
	}

	private static int getFreePort() throws IOException {
		try (ServerSocket socket = new ServerSocket(0)) {
			socket.setReuseAddress(true);
			return socket.getLocalPort();
		}
	}
}
