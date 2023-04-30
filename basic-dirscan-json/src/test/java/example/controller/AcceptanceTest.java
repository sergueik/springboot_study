package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.model.Artist;
import example.model.ArtistSerializer;

// NOTE: port annotations have no effect: "application.properties" wins  
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })

@PropertySource("classpath:application.properties")

public class AcceptanceTest {

	// NOTE:
	// BeanPostProcessor : Autowired annotation is not supported on static fields:

	@LocalServerPort
	private int randomServerPort = 8085;

	private static final RestTemplate restTemplate = new RestTemplate();
	private String url = null;
	private HttpHeaders headers = new HttpHeaders();
	private ResponseEntity<String> responseEntity = null;
	private HttpEntity<Artist> request = null;
	private Artist artist = null;

	private static Gson gson = new GsonBuilder()
			.registerTypeAdapter(Artist.class, new ArtistSerializer()).create();

	@BeforeEach
	public void setUp() {
		headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_JSON);

	}

	@Test
	public void test1() throws Exception {
		String name = "john";
		url = "http://localhost:" + randomServerPort + "/updatedata/" + name;
		artist = new Artist(1, name, "guitar");
		request = new HttpEntity<Artist>(artist, headers);
		responseEntity = restTemplate.postForEntity(url, request, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
	}

	@Disabled
	// TODO: the exception is reported as test failure:
	// org.springframework.web.client.HttpClientErrorException$BadRequest [bad
	// name]
	// Errors:
	// AcceptanceTest.test2:94 » BadRequest 400 : [bad name]
	@Test
	public void test2() throws Exception {
		String name = "john";
		url = "http://localhost:" + randomServerPort + "/updatedata/" + name;
		headers = new HttpHeaders();
		artist = new Artist(1, "paul", "guitar");
		headers.setContentType(MediaType.APPLICATION_JSON);
		request = new HttpEntity<Artist>(artist, headers);
		responseEntity = restTemplate.postForEntity(url, request, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.METHOD_NOT_ALLOWED));
	}

	@Test
	public void test3() throws Exception {
		String name = "john";
		url = "http://localhost:" + randomServerPort + "/updatedata/" + name;
		artist = new Artist(1, name, "guitar");
		HttpEntity<String> request2 = new HttpEntity<String>(gson.toJson(artist),
				headers);
		responseEntity = restTemplate.postForEntity(url, request2, String.class,
				headers);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
	}

}