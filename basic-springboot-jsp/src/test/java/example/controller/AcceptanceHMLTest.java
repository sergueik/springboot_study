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

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.web.server.LocalServerPort;
import org.springframework.context.annotation.PropertySource;

import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.ResponseEntity;
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
		page = getHtmlPage(base_url, responseEntity.getBody());
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

	private HtmlPage getHtmlPage(String url, String payload) throws IOException {
		StringWebResponse response = new StringWebResponse(payload,
				new URL("http://localhost:8080"));
		WebClient client = new WebClient();
		HtmlPage page = new HtmlUnitNekoHtmlParser()
				.parseHtml((WebResponse) response, client.getCurrentWindow());
		client.close();
		return page;
	}

}
