package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Assumptions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.http.HttpStatus;

import org.springframework.web.client.HttpClientErrorException;
import org.springframework.web.client.RestTemplate;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.net.URL;

import java.util.ArrayList;
import java.util.List;

import java.io.IOException;
import static org.springframework.http.HttpStatus.BAD_REQUEST;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;

import com.gargoylesoftware.htmlunit.StringWebResponse;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.WebResponseData;
import com.gargoylesoftware.htmlunit.html.DomElement;
import com.gargoylesoftware.htmlunit.html.DomNode;
import com.gargoylesoftware.htmlunit.html.DomNodeList;
import com.gargoylesoftware.htmlunit.html.HTMLParser;
import com.gargoylesoftware.htmlunit.html.HtmlAnchor;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlInput;
import com.gargoylesoftware.htmlunit.html.HtmlPage;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8085" })
@PropertySource("classpath:application.properties")
public class AcceptanceTest {

	// NOTE: BeanPostProcessor :
	// Autowired annotation is not supported on static fields:

	@LocalServerPort
	private int serverPort = 8085;

	private final String route = "/";
	// NOTE: exercising property file override
	private final static String body = "Hello World";
	private static final RestTemplate restTemplate = new RestTemplate();
	// cannot initialize too early ?
	private String url = null; 
	private HttpHeaders headers = new HttpHeaders();
	private HttpEntity<String> request = null;
	private ResponseEntity<String> responseEntity = null;
	private static HtmlPage page;

	@BeforeEach
	public void setUp() {
		url =  "http://localhost:" + serverPort + route;
	}

	@Test
	public void test1() throws Exception {
		// Assumptions.assumeFalse(false);
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getStatusCode(), is(HttpStatus.OK));
	}

	@Test
	public void test2() throws Exception {
		// Assumptions.assumeFalse(false);
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getBody(), containsString(body));
	}

	@Test
	public void test3() throws Exception {
		// Assumptions.assumeFalse(false);
		String name = "value";
		responseEntity = restTemplate.getForEntity(url + "?name=" + name, String.class);
		assertThat(responseEntity.getBody(), containsString(String.format("Hello %s", name)));
	}

	@Test
	public void test4() throws Exception {
		String id = "0";
		responseEntity = restTemplate.getForEntity(url, String.class);
		page = getHtmlPage(responseEntity.getBody());
		assertThat(page, notNullValue());
		DomElement element = (page.getElementsById(id).get(0));
		assertThat(element, notNullValue());
		assertThat(element.getTextContent(), containsString(body));
	}

	private HtmlPage getHtmlPage(String page) throws IOException {
		StringWebResponse response = new StringWebResponse(page, new URL(url));
		WebClient client = new WebClient();
		return HTMLParser.parseHtml(response, client.getCurrentWindow());
	}
}
