package example.controller;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

@WebMvcTest
public class ControllerTest {
	final static String route = "/basic/upload";
	private static MockMvc mvc;

	private static Controller controller = new Controller();
	private static MockMultipartFile file;

	@BeforeClass
	public static void setUp() {
		mvc = MockMvcBuilders.standaloneSetup(controller).build();
		file = new MockMultipartFile("file", "test.txt", MediaType.TEXT_PLAIN_VALUE,
				"Hello, World!".getBytes());
	}

	// examine response status and body
	@Test
	public void test() throws Exception {
		mvc.perform(multipart(route).file(file)).andExpect(status().isOk())
				.andExpect(content().string(""));
	}
	// NOTE: cannot perform console capture test dure to spring boot version
	// conflicts:
	// in older one there is no multipart method,
	// in newer no capture
	// class
}
