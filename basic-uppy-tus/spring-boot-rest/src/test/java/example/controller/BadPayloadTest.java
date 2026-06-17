package example.controller;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.stream.Stream;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.boot.test.context.SpringBootTest;

import static java.net.http.HttpRequest.newBuilder;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8080" })

public class BadPayloadTest {
	private int randomServerPort = 8080;
	private HttpRequest request = null;
	private HttpResponse<byte[]> response = null;
	private byte[] data = "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
			.getBytes();
	private BufferedImage img = null;
	private static String url = null;
	private final static String route = "/api/upload";
	private TusFileUploadController controller;

	@DisplayName("The Upload-Offset was null but expected 0")
	@Test
	public void test1() throws Exception {
		String url = "http://localhost:" + randomServerPort + route;
		request = HttpRequest.newBuilder().uri(URI.create(url))
				.method("POST", HttpRequest.BodyPublishers.ofByteArray(new byte[] {}))
				.header("Upload-Defer-Length", "1").header("Tus-Resumable", "1.0.0").build();
		response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofByteArray());

		assertThat(response.statusCode(), is(201));
		System.err.println(response.headers().firstValue("location"));
		String location = response.headers().firstValue("location").get();
		url = "http://localhost:" + randomServerPort + location;
		request = HttpRequest.newBuilder().uri(URI.create(url))
				.method("PATCH", HttpRequest.BodyPublishers.ofByteArray(data)).header("Tus-Resumable", "1.0.0")
				.header("Content-Type", "application/offset+octet-stream").build();

		response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofByteArray());

		assertThat(response.statusCode(), is(409));

		byte[] body = response.body();
		assertThat(body, notNullValue());
	}

	@Disabled("IllegalArgument: restricted header name: Content-Length")
	@Test
	public void test2() throws Exception {
		String url = "http://localhost:" + randomServerPort + route;
		request = HttpRequest.newBuilder().uri(URI.create(url))
				.method("POST", HttpRequest.BodyPublishers.ofByteArray(new byte[] {}))
				.header("Upload-Defer-Length", "1").header("Tus-Resumable", "1.0.0").build();
		response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofByteArray());

		assertThat(response.statusCode(), is(201));
		System.err.println(response.headers().firstValue("location"));
		String location = response.headers().firstValue("location").get();
		url = "http://localhost:" + randomServerPort + location;
		request = HttpRequest.newBuilder().uri(URI.create(url))
				.method("PATCH", HttpRequest.BodyPublishers.ofByteArray(data)).header("Tus-Resumable", "1.0.0")
				.header("Content-Type", "application/offset+octet-stream").header("Upload-Offset", "0")
				.header("Content-Length", "1024").build();

		response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofByteArray());

		assertThat(response.statusCode(), is(200));

		byte[] body = response.body();
		assertThat(body, notNullValue());
	}
}
