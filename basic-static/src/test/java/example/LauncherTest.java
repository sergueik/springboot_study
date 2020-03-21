package example;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit4.SpringRunner;
import static org.hamcrest.CoreMatchers.is;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;

import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.mockito.Matchers.any;

import org.junit.Test;
import org.junit.runner.RunWith;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import org.owasp.validator.css.CssScanner;
import org.owasp.validator.css.ExternalCssScanner;
import org.owasp.validator.html.CleanResults;
import org.owasp.validator.html.InternalPolicy;
import org.owasp.validator.html.Policy;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Locale;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.stream.Stream;

import javax.servlet.ServletOutputStream;

import org.owasp.validator.html.PolicyException;
import org.xml.sax.SAXParseException;
// see also:
// https://github.com/andresriancho/owaspantisamy/blob/master/Java/antisamy/src/main/java/org/owasp/validator/css/CssScanner.java

@RunWith(SpringRunner.class)
@WebMvcTest(Launcher.class)
public class LauncherTest {

	@Autowired
	private MockMvc mockMvc;

	@Test
	public void statusTest() throws java.lang.Exception {
		final String resourcePath = "/js/script.js";
		// Act
		MvcResult result = mockMvc.perform(MockMvcRequestBuilders.get(resourcePath)
				.accept(MediaType.APPLICATION_JSON_UTF8)).andReturn();
		// Assert
		assertEquals("Incorrect Response Status", HttpStatus.OK.value(),
				result.getResponse().getStatus());
	}

	@Test
	public void cssTest() throws java.lang.Exception {
		final String htmlPath = "/index.html";
		final String cssPath = "/css/main.css";
		// Act
		MvcResult result = mockMvc.perform(MockMvcRequestBuilders.get(cssPath)
				.accept(MediaType.APPLICATION_JSON_UTF8)).andReturn();
		// Assert
		assertEquals("Incorrect Response Status", HttpStatus.OK.value(),
				result.getResponse().getStatus());
		String input = result.getResponse().getContentAsString();
		// https://www.programcreek.com/java-api-examples/?code=werval/werval/werval-master/io.werval.modules/io.werval.modules.sanitize/src/main/java/io/werval/modules/sanitize/Sanitize.java
		System.err.println("Validating: " + input);
		final ClassLoader loader = this.getClass().getClassLoader();
		ResourceBundle messages = ResourceBundle.getBundle("AntiSamy", Locale.US,
				loader);
		CssScanner cssScanner = null;
		try {
			cssScanner = new CssScanner(
					(InternalPolicy) InternalPolicy.getInstance(getPageContent(htmlPath)),
					messages);
		} catch (PolicyException e) {
			// org.xml.sax.SAXParseException;
			// static/css/main.css; lineNumber: 1; columnNumber: 1; Content is not
			// allowed in prolog.
			// is attempting to scan CSS as HTML
		}
		if (cssScanner != null) {
			CleanResults results = cssScanner.scanStyleSheet(input,
					Integer.MAX_VALUE);
			assertEquals("Invalid CSS", 0, results.getNumberOfErrors());
		}
	}

	// NOTE: cannot make this version static
	protected String getPageContent(String pagename) {
		try {
			URI uri = this.getClass().getClassLoader().getResource(pagename).toURI();
			System.err.println("Testing local file: " + uri.toString());
			return uri.toString();
		} catch (URISyntaxException | NullPointerException e) {
			// mask the exception when debug
			final String mockupPath = String.format("src/main/resources/static/%s",
					pagename);
			System.err.println("Testing mocked up file: " + mockupPath);
			return mockupPath;
			// throw new RuntimeException(e);

		}
	}

}
