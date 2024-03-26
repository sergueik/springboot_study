package example;

import static org.junit.Assert.assertEquals;

import java.io.File;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Locale;
import java.util.ResourceBundle;

import org.junit.Assume;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import org.springframework.test.context.junit4.SpringRunner;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

@RunWith(SpringRunner.class)
@WebMvcTest(Launcher.class)
public class LauncherTest {

	private MvcResult result = null;

	// private static final boolean debug = false;

	@Value("${debug:false}")
	boolean debug;

	@Autowired
	private MockMvc mockMvc;

	@Test
	public void statusTest() throws java.lang.Exception {
		// Act
		result = mockMvc.perform(MockMvcRequestBuilders.get("/js/script.js")
				.accept(MediaType.APPLICATION_JSON_UTF8)).andReturn();
		// Assert
		assertEquals("Unexpected response status", HttpStatus.OK.value(),
				result.getResponse().getStatus());
	}

	@Test
	// NOTE: will pass in presednce of example.config.Config
	public void statusTest2() throws java.lang.Exception {
		Assume.assumeTrue(new File(String.format("%s/src/main/java/%s",
				System.getProperty("user.dir"), "example/config/Config.java"))
						.exists());
		result = mockMvc
				.perform(MockMvcRequestBuilders.get("/webjars/jquery/%s/jquery.min.js"))
				.andReturn();
		assertEquals("Unexpected response status", HttpStatus.NOT_FOUND.value(),
				result.getResponse().getStatus());
	}

	// @Ignore
	// NOTE: will pass in absence of example.config.Config
	@Test
	public void statusTest3() throws java.lang.Exception {
		Assume.assumeFalse(new File(String.format("%s/src/main/java/%s",
				System.getProperty("user.dir"), "example/config/Config.java"))
						.exists());
		final String version = "3.2.1";
		result = mockMvc.perform(MockMvcRequestBuilders
				.get(String.format("/webjars/jquery/%s/jquery.min.js", version))
				.accept(MediaType.APPLICATION_JSON_UTF8)).andReturn();
		assertEquals("Unexpected response status", HttpStatus.OK.value(),
				result.getResponse().getStatus());
	}

	@Test
	public void statusTest4() throws java.lang.Exception {
		// Act
		result = mockMvc
				.perform(MockMvcRequestBuilders.get("/templates/index.html"))
				.andReturn();
		// Assert
		assertEquals("Unexpected response status", HttpStatus.NOT_FOUND.value(),
				result.getResponse().getStatus());
	}

	// NOTE: cannot make this version static
	protected String getPageContent(String pagename) {
		final String resourcePath = "src/main/resources/static";
		try {
			URI uri = this.getClass().getClassLoader().getResource(pagename).toURI();

			if (debug) {
				System.err.println("Testing local file: " + uri.toString());
			}
			return uri.toString();
		} catch (URISyntaxException | NullPointerException e) {
			// mask the exception when debug
			if (debug) {
				System.err
						.println("Testing mocked up file: " + resourcePath + pagename);
			}
			return resourcePath + pagename;
			// throw new RuntimeException(e);
		}
	}

}
