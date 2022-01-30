package example.controller;

import static org.hamcrest.Matchers.containsString;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.Application;
import example.model.MalformedUser;

// @PropertySource("classpath:application.properties")
@WebMvcTest
public class ControllerTest {

	final static String route = "/user";
	final static String body = "Hello basic";
	private static String charset = null;
	private ResultActions resultActions;
	private static MockMvc mvc;

	// initiaize real stuff
	@SuppressWarnings("unused")
	private static Application application = new Application();

	private static FieldValidationController controller = new FieldValidationController();

	private static final Gson gsonPrinter = new GsonBuilder().setPrettyPrinting()
			.create();

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
	}

	private MalformedUser user;

	@Before
	public void beforeTest() throws Exception {
		user = new MalformedUser("name", "name@server.com", "a1b2c3d4");
	}

	@Test
	public void test1() throws Exception {
		resultActions = mvc.perform(
				post(route).contentType(MediaType.APPLICATION_JSON).content("{}"));
		resultActions
				.andExpect(content().string(containsString("may not be null")));
	}

	@Test
	public void test2() throws Exception {

		user.setName("");
		resultActions = mvc
				.perform(post(route).contentType(MediaType.APPLICATION_JSON)
						.content(gsonPrinter.toJson(user)));
		resultActions.andExpect(content()
				.string(containsString("Name should contains more than 2 character")));
	}

	@Test
	public void test3() throws Exception {

		user.setName(null);
		resultActions = mvc
				.perform(post(route).contentType(MediaType.APPLICATION_JSON)
						.content(gsonPrinter.toJson(user)));
		resultActions
				.andExpect(content().string(containsString("may not be null")));
	}

	@Test
	public void test4() throws Exception {

		user.setEmail("invalid email");
		resultActions = mvc
				.perform(post(route).contentType(MediaType.APPLICATION_JSON)
						.content(gsonPrinter.toJson(user)));
		resultActions.andExpect(content().string(containsString("Invalid Email")));
	}

	@Test
	public void test5() throws Exception {
		user.setPassword("");
		resultActions = mvc
				.perform(post(route).contentType(MediaType.APPLICATION_JSON)
						.content(gsonPrinter.toJson(user)));
		resultActions
				.andExpect(content()
						.string(containsString("Character should be in between 6 to 8")))
				.andExpect(content().string(containsString("Mandatory field")));
	}

	@Test
	public void test7() throws Exception {

		resultActions = mvc
				.perform(post(route).contentType(MediaType.APPLICATION_JSON)
						.content(gsonPrinter.toJson(user)));
		resultActions.andExpect(content().string(containsString("User inserted")));
	}

}
