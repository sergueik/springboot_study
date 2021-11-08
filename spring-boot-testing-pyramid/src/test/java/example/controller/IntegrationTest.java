package example.controller;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.web.servlet.MockMvc;

import example.service.ExampleService;

import static org.hamcrest.Matchers.equalTo;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

@WebMvcTest
class IntegrationTest {

	@Autowired
	private MockMvc mvc;

	@MockBean
	private ExampleService mockService;

	@Test
	void test1() throws Exception {
		when(mockService.hello()).thenReturn("Welcome mock");
		mvc.perform(get("/basic")).andDo(print()).andExpect(status().isOk())
				.andExpect(content().string(equalTo("Welcome mock")));
		verify(mockService).hello();
	}
}
