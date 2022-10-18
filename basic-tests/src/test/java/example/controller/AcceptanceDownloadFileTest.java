package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.collection.IsArrayWithSize.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assumptions;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpMethod;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/test/web/servlet/result/JsonPathResultMatchers.html
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
// requires a later version ?
// import static org.springframework.test.web.servlet.result.JsonPathResultMatchers.isArray;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import example.controller.ExampleController;
import example.service.ExampleService;
import example.Application;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")

// based on
// https://www.javacodemonk.com/download-a-file-using-spring-resttemplate-75723d97
// see also:
// https://javadeveloperzone.com/spring-boot/spring-boot-resttemplate-download-file-example/
// see also:
// https://www.baeldung.com/spring-resttemplate-download-large-file
public class AcceptanceDownloadFileTest {

	@LocalServerPort
	private int randomServerPort = 8085;

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
		headers.setAccept(Arrays.asList(MediaType.APPLICATION_OCTET_STREAM));
		entity = new HttpEntity<>(headers);

	}

	// examine HTTP status and body - missing request params
	@Test
	public void test1() throws Exception {
		try {
			route = "/basic/download/resource" + "?" + "resourceFileName=test.txt";
			url = "http://localhost:" + randomServerPort + route;
			response = restTemplate.build().exchange(url, HttpMethod.GET, entity,
					byte[].class);
			Files.write(Paths.get(
					"target" + System.getProperty("file.separator") + "test1.result.txt"),
					response.getBody());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	@Test
	public void test2() throws Exception {
		try {
			route = "/basic/download/file";
			url = "http://localhost:" + randomServerPort + route;
			response = restTemplate.build().exchange(url, HttpMethod.GET, entity,
					byte[].class);
			Files.write(Paths.get(
					"target" + System.getProperty("file.separator") + "test2.result.txt"),
					response.getBody());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

}
