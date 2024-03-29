package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;

import java.io.IOException;
import java.net.URL;
import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.util.LinkedMultiValueMap;
import org.springframework.util.MultiValueMap;
import org.springframework.web.client.RestTemplate;

import com.gargoylesoftware.htmlunit.StringWebResponse;
import com.gargoylesoftware.htmlunit.WebClient;
import com.gargoylesoftware.htmlunit.WebResponse;
import com.gargoylesoftware.htmlunit.html.DomElement;
import com.gargoylesoftware.htmlunit.html.DomNode;
import com.gargoylesoftware.htmlunit.html.DomNodeList;
import com.gargoylesoftware.htmlunit.html.HtmlElement;
import com.gargoylesoftware.htmlunit.html.HtmlPage;
import com.gargoylesoftware.htmlunit.html.parser.neko.HtmlUnitNekoHtmlParser;

// NOTE: property annotations have no effect
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = {
		"serverPort=8085" })
@PropertySource("classpath:application.properties")
public class AcceptanceHMLTest {

	// @LocalServerPort
	// private static final int serverPort = 8085;
	// private static String url;
	// private static final String route = "/model";

	@LocalServerPort
	// NOTE: cannot annotate "final", or static and use in @BeforeAll:
	// context
	// org.springframework.web.client.ResourceAccessException: I/O error on GET
	// request for "http://localhost:8085/model": Connection refused: connect;
	// nested exception is java.net.ConnectException: Connection refused:
	// connect

	private /* final */ int serverPort = 8085;
	private String base_url;
	private final String route = "/model";

	// NOTE: exercising property file override
	private final static String text = "Hello World";
	private static final RestTemplate restTemplate = new RestTemplate();
	private ResponseEntity<String> responseEntity = null;
	private static HtmlPage page;
	private static HtmlElement documentElement;
	private static HtmlElement element;
	private static DomElement domElement;
	private static DomNode domNode;
	private static DomNodeList<HtmlElement> elements;
	private static DomNodeList<DomNode> domElements;
	private static String id = "0";

	@BeforeAll
	public static void setAll() {
		// NOTE: cannot reference instance vars notably serverPort, from a static
		// base_url = "http://localhost:" + serverPort + route;
	}

	@BeforeEach
	public void setUp() throws IOException {
		base_url = "http://localhost:" + serverPort + route;
		responseEntity = restTemplate.getForEntity(base_url, String.class);
		page = getHtmlPage(responseEntity.getBody());
	}

	@Test
	public void test1() throws Exception {
		assertThat(page, notNullValue());
		domElement = page.getElementsById(id).get(0);
		assertThat(domElement, notNullValue());
		assertThat(domElement.getTextContent(), containsString(text));
	}

	@Test
	public void test2() throws Exception {

		assertThat(page, notNullValue());
		documentElement = page.getDocumentElement();
		assertThat(documentElement, notNullValue());
		elements = documentElement.getElementsByTagName("div");
		assertThat(elements.size(), greaterThan(0));
		element = elements.get(0);
		assertThat(element.getTextContent(), containsString(text));
	}

	@Test
	public void test3() throws Exception {

		assertThat(page, notNullValue());
		domElements = page.querySelectorAll(String.format("div#%s", id));
		assertThat(domElements, notNullValue());
		assertThat(domElements.size(), greaterThan(0));
		domNode = domElements.get(0);
		assertThat(domNode, notNullValue());
		assertThat(domNode.asText(), containsString(text));

	}

	@Test
	public void test4() throws Exception {
		// Assumptions.assumeFalse(false);
		String name = "value";
		String url = base_url + "?name=" + name;
		responseEntity = restTemplate.getForEntity(url, String.class);
		assertThat(responseEntity.getBody(),
				containsString(String.format("Hello %s", name)));
	}

	@Disabled("BadRequest 400, wrong arguments")
	@Test
	public void test5() throws Exception {
		String name = "value";
		String id = "1";
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		Map<String, String> param = new HashMap<>();
		param.put("name", name);
		param.put("id", id);
		// NOTE: cannot use plain Map here
		// org.springframework.web.client.RestClientException: No
		// HttpMessageConverter for java.util.HashMap and content type
		// "application/x-www-form-urlencoded"
		HttpEntity<Map<String, String>> request = new HttpEntity<>(param, headers);
		responseEntity = restTemplate.postForEntity(base_url, request,
				String.class);
		assertThat(responseEntity.getBody(),
				containsString(String.format("Hello %s", name)));
	}

	@Test
	public void test6() throws Exception {
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		String name = "value";
		String id = "1";

		MultiValueMap<String, String> param = new LinkedMultiValueMap<>();
		param.add("name", name);
		param.add("id", id);
		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(param,
				headers);

		responseEntity = restTemplate.postForEntity(base_url, request,
				String.class);

		assertThat(responseEntity.getBody(),
				containsString(String.format("Hello %s", name)));
	}

	@Test
	public void test7() throws Exception {
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.APPLICATION_FORM_URLENCODED);
		String name = "value";
		String id = "1";

		MultiValueMap<String, String> param = new LinkedMultiValueMap<>();
		param.add("name", name);
		param.add("id", id);
		// HttpEntity<Map<String, String>> request = new HttpEntity<>(param);
		HttpEntity<MultiValueMap<String, String>> request = new HttpEntity<>(param,
				headers);

		responseEntity = restTemplate.postForEntity(base_url, request,
				String.class);
		page = getHtmlPage(responseEntity.getBody());

		domElement = page.getElementsById(id).get(0);
		assertThat(domElement, notNullValue());
		assertThat(domElement.getTextContent(),
				containsString(String.format("Hello %s", name)));

	}

	// NOTE: not using the url parameter
	private HtmlPage getHtmlPage(String payload) throws IOException {
		StringWebResponse response = new StringWebResponse(payload,
				new URL("http://localhost:8080"));
		WebClient client = new WebClient();
		HtmlPage page = new HtmlUnitNekoHtmlParser()
				.parseHtml((WebResponse) response, client.getCurrentWindow());
		client.close();
		return page;
	}

}
