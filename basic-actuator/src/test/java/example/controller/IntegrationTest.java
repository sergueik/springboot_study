package example.controller;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.contains;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import com.google.gson.Gson;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8085" })
@PropertySource("classpath:application.properties")

// based on
// https://www.javacodemonk.com/download-a-file-using-spring-resttemplate-75723d97
// see also:
// https://javadeveloperzone.com/spring-boot/spring-boot-resttemplate-download-file-example/
// see also:
// https://www.baeldung.com/spring-resttemplate-download-large-file
public class IntegrationTest {

	@LocalServerPort
	private int randomServerPort = 8085;
	private static final Gson gson = new Gson();
	@Autowired
	private RestTemplateBuilder restTemplate;
	static String route = null;
	final static String body = "Hello basic";
	private String url = null;
	private ResponseEntity<byte[]> response = null;
	private HttpEntity<String> entity = null;
	private HttpHeaders headers = null;

	@BeforeAll
	public static void setUp() {
	}

	@BeforeEach
	public void beforeTest() throws Exception {
		headers = new HttpHeaders();
		headers.setAccept(Arrays.asList(MediaType.APPLICATION_JSON));
		entity = new HttpEntity<>(headers);

	}

	@Test
	public void test1() throws Exception {
		try {
			route = "/actuator";
			url = "http://localhost:" + randomServerPort + route;
			response = restTemplate.build().exchange(url, HttpMethod.GET, entity, byte[].class);
			String body = new String(response.getBody());
			assertThat(body, notNullValue());
			System.err.println("BODY: " + body);
			@SuppressWarnings("unchecked")
			Map<String, Object> data = gson.fromJson(body, Map.class);
			assertThat(data.keySet(), notNullValue());
			assertThat(data.keySet().size(), is(1));
			assertThat(data.keySet(),contains("_links"));
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void test2() throws Exception {
		try {
			route = "/actuator";
			url = "http://localhost:" + randomServerPort + route;
			response = restTemplate.build().exchange(url, HttpMethod.GET, entity, byte[].class);
			int statusCode = response.getStatusCode().value();
			assertThat(statusCode, is(200));
			System.err.println("STATUS: " + response.getStatusCode());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
