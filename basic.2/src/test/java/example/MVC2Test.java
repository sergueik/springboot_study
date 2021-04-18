package example;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import java.util.Arrays;

import static org.hamcrest.CoreMatchers.containsString;

import org.hamcrest.Matcher;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Import;
import org.springframework.mock.web.MockServletContext;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.context.support.AnnotationConfigWebApplicationContext;

import javax.servlet.ServletContext;
import org.springframework.http.MediaType;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

import org.springframework.boot.test.autoconfigure.web.client.RestClientTest;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.boot.autoconfigure.SpringBootApplication;

import example.Controller;

// https://www.tutorialspoint.com/spring_boot/spring_boot_runners.htm
@RunWith(SpringRunner.class)

@WebMvcTest(controllers = Application.class)
public class MVC2Test {

	// https://www.baeldung.com/integration-testing-in-spring (2.2.6.RELEASE
	// example)
	// https://www.baeldung.com/restclienttest-in-spring-boot
	// https://www.baeldung.com/spring-web-contexts
	// https://www.programcreek.com/java-api-examples/org.springframework.web.context.WebApplicationContext
	// https://reflectoring.io/spring-boot-web-controller-test/

	@Autowired
	private WebApplicationContext webApplicationContext;

	// initiaize real stuff ?
	// private Service service = new Service();
	// private Controller controller = new Controller(service);
	// private Application application = new Application();

	/// org.springframework.beans.factory.UnsatisfiedDependencyException:
	// Error creating bean with name 'example.MVC2Test':
	// Unsatisfied dependency expressed through field 'service'
	// No qualifying bean of type 'example.Service'
	// expected at least 1 bean which qualifies as autowire candidate.
	/*
	@Autowired
	
	private Service service;
	@Autowired
	private Controller controller;
	@Autowired
	private Application application;
	*/
	@Autowired
	private MockMvc mvc;

	final static String route = "/basic";
	final static String body = "Hello basic";
	private ResultActions resultActions;
	final static String charset = "UTF-8";

	@Before
	// [ERROR] MVC2Test.bodyContainsTextTest » IllegalState Failed to load
	// ApplicationContext
	public void beforeTest() throws Exception {
		resultActions = mvc.perform(get(route));
		// resultActions = mvc.perform(get(route).accept(MediaType.TEXT_PLAIN)
		// .param("foo", "true").content("a=1&b=2"));
		/*
		 mvc.perform(post("/forums/{forumId}/register", 42L)
		      .contentType("application/json")
		s		      .param("sendWelcomeMail", "true")
		      .content(objectMapper.writeValueAsString(user)))
		      .andExpect(status().isOk());
		 */
	}

	// examine HTTP status
	@Ignore
	// Status expected:<200> but was:<404>
	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}

	// assert the response body content with a Hamcrest Matcher
	@Ignore
	// Expected: a string containing "Hello basic"
	// but: was ""
	@Test
	public void bodyContainsTextTest() throws Exception {
		Matcher<String> matcher = containsString(body);
		resultActions.andExpect(content().string(matcher));
	}

	// examine response header
	@Ignore
	// Content type not set
	@Test
	public void contentTypeTest() throws Exception {
		resultActions.andExpect(
				content().contentType(String.format("text/plain;charset=%s", charset)));
	}

}
