package example;

import org.junit.Before;
import org.junit.Test;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

public class MVCTest {

	final static String route = "/basic";
	final static String body = "Hello basic";
	final static String contentType = "text/plain;charset=ISO-8859-1";
	final static String charset = "ISO-8859-1";

	private MockMvc mvc;
	
	@Before
	public void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(new ExampleApplication()).build();
	}


	@Test
	public void statusTest() throws Exception {
		mvc.perform(get(route)).andExpect(status().isOk());
	}

	@Test
	public void contentTest() throws Exception {
		mvc.perform(get(route)).andExpect(content().string(body));
	}


	@Test
	public void contentTypeTest() throws Exception {
		mvc.perform(get(route).accept(MediaType.TEXT_PLAIN)).andExpect(
				content().contentType(String.format("text/plain;charset=%s", charset)));
		mvc.perform(get(route).accept(MediaType.APPLICATION_JSON))
				.andExpect(content().contentType(
						String.format("application/json;charset=%s", charset)));
	}
}