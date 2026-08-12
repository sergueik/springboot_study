package example.controller;
/**
 * Copyright 2022,2026 Serguei Kouzmine
 */

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URISyntaxException;
import java.nio.file.Files;
import java.util.Arrays;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;

import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.containsString;

import example.controller.HomeController;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = HomeController.class)
public class HomeControllerTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	final static String charset = "UTF-8";
	final static String body = "<html/>";
	@Value("${view:home}")
	private String viewName;

	// NOTE: "application" is a reserved variable name
	@Value("${application}")
	private String variable;

	@Value("${title:title not set}")
	private String title;

	@Before
	public void beforeTest() throws Exception {
		resultActions = mvc.perform(get("/" + variable).accept(MediaType.TEXT_HTML));
	}

	@Test
	public void errorTest() throws Exception {
		resultActions.andExpect(model().hasNoErrors());
	}

	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	// assert the response body content with a Hamcrest Matcher
	@Test
	public void bodyContainsTemplatLayoutTextTest() throws Exception {
		resultActions.andExpect(content().string(containsString(String.format("<title>%s</title>", title))));
	}
	// assert the response body not the content with a Hamcrest Matcher

	@Test
	public void bodyNotContainsTemplateBoilleplateTextTest() throws Exception {
		resultActions.andExpect(content().string(not(containsString("layout:decorate=\"~{layouts/layout}\""))));
	}

	@Test
	public void bodyNotContainsTemplateResourceBoilleplateTextTest() throws Exception {
		var resource = getScriptContent(String.format("templates/%s.html", viewName));
		resultActions.andExpect(content().string(not(containsString(resource.split("\\n")[1]))));
	}

	@Test
	public void bodyNotContainsTemplateResourceURIBoilleplateTextTest() throws Exception {
		var resource = Files
				.readAllLines(new File(getResourcePath(String.format("templates/%s.html", viewName))).toPath()).get(1);
		resultActions.andExpect(content().string(not(containsString(resource))));
	}

	@Test
	public void contentTypeTest() throws Exception {
		resultActions.andExpect(content().contentType(String.format("text/html;charset=%s", charset)));
	}

	@Test
	public void veiewNameTest() throws Exception {
		resultActions.andExpect(view().name(viewName));
	}

	@Test
	public void verifiesHomePageLoads() throws Exception {

		Arrays.asList("variable", "hostname", "now").forEach(a -> {
			try {
				resultActions.andExpect(model().attributeExists(a));
			} catch (Exception e) {
				// for missing attributes, the
				// java.lang.AssertionError will be thrown:
				// Model attribute 'then' does not exist
				return;
			}
		});
	}

	// NOTE: put inside "WEB-INF/classes" for web hosted app
	public String getScriptContent(String resourceFileName) {
		try {
			System.err.println("Script contents: " + getResourceURI(resourceFileName));
			final InputStream stream = getResourceStream(resourceFileName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
	}

	// NOTE: getResourceURI may not work with standalone or web hosted
	// application
	public String getResourceURI(String resourceFileName) {
		try {
			System.err.println("Getting resource URI for: " + resourceFileName);
			URI uri = this.getClass().getClassLoader().getResource(resourceFileName).toURI();
			System.err.println("Resource URI: " + uri.toString());
			return uri.toString();
		} catch (URISyntaxException e) {
			throw new RuntimeException(e);
		}
	}

	public InputStream getResourceStream(String resourceFilePath) {
		return this.getClass().getClassLoader().getResourceAsStream(resourceFilePath);
	}

	public String getResourcePath(String resourceFileName) {
		final String resourcePath = String.format("%s/src/main/resources/%s", System.getProperty("user.dir"),
				resourceFileName);
		System.err.println("Project based resource path: " + resourcePath);
		return resourcePath;
	}

}
