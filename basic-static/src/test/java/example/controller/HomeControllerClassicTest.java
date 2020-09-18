package example.controller;

import java.util.Arrays;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.result.MockMvcResultMatchers;

import example.controller.HomeController;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = HomeController.class)
public class HomeControllerClassicTest {

	@Autowired
	private MockMvc mockMvc;

	@Value("${application}")
	// NOTE: application is a reserved variable name
	private String variable;

	// System.getProperty("application");
	@Test
	public void verifiesHomePageLoads() throws Exception {
		ResultActions o = mockMvc.perform(MockMvcRequestBuilders.get("/" + variable));
		o.andExpect(MockMvcResultMatchers.model().hasNoErrors()).andExpect(MockMvcResultMatchers.view().name("index"))
				.andExpect(MockMvcResultMatchers.status().isOk());
		Arrays.asList("variable", "hostname", "now").forEach(a -> {
			try {
				o.andExpect(MockMvcResultMatchers.model().attributeExists(a));
			} catch (Exception e) {
				// for missing attributes, the
				// java.lang.AssertionError will be thrown:
				// Model attribute 'then' does not exist
				return;
			}
		});
	}

}
