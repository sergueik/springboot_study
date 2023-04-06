package example.controller;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest
@DirtiesContext(classMode = DirtiesContext.ClassMode.BEFORE_EACH_TEST_METHOD)
@AutoConfigureMockMvc
public class HackerRankTest {

	@Autowired
	private MockMvc mockMvc;

	@Before
	public void setup() {
	}

	@Test
	public void defaultHelloTest() throws Exception {
		mockMvc.perform(get("/basic/defaultHello")).andDo(print())
				.andExpect(jsonPath("$.echo").value("Hello World!"))
				.andExpect(status().isOk());
	}

	@Test
	public void defaultHelloWithParamTest() throws Exception {
		mockMvc.perform(get("/basic/defaultHello").param("message", "HackerRank!"))
				.andDo(print()).andExpect(jsonPath("$.echo").value("Hello HackerRank!"))
				.andExpect(status().isOk());
	}

	@Test
	public void customHelloTest() throws Exception {
		String message = "Custom Hello World!";
		mockMvc.perform(post("/basic/customHello").param("message", message))
				.andDo(print()).andExpect(jsonPath("$.echo").value(message))
				.andExpect(status().isOk());
	}

	@Test
	public void customHelloWithEmptyParamTest() throws Exception {
		mockMvc.perform(post("/basic/customHello").param("message", ""))
				.andDo(print()).andExpect(jsonPath("$.echo").value("Custom "))
				.andExpect(status().isOk());
	}

	@Test
	public void customHelloWithoutParamTest() throws Exception {
		mockMvc.perform(post("/basic/customHello")).andDo(print())
				.andExpect(status().isBadRequest());
	}

	@Test
	public void customPathTest() throws Exception {
		mockMvc.perform(post("/basic/pathHello")).andDo(print())
				.andExpect(status().isNotFound());
	}
}
