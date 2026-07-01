package example.controller;

/* Copyright 2026 Serguei Kouzmine */

// import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.mockito.InjectMocks;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.ContextConfiguration;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.ResultActions;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;

import example.service.DotnetConfigService;
import static org.mockito.Mockito.when;
import static org.hamcrest.Matchers.containsString;

@SpringBootTest
@AutoConfigureMockMvc
// NOTE: too restrictive - prone to lead to error in the test
// No converter for [class java.util.HashMap] with preset Content-Type 'null'
// because Spring never allowed to select MappingJackson2HttpMessageConverter 
/*
@ContextConfiguration(classes = { DotnetConfigService.class, DotnetConfigController.class,
		com.fasterxml.jackson.databind.ObjectMapper.class })
		*/
public class DotnetConfigControllerTest {

	@Autowired
	MockMvc mockMvc;

	// NOTE: risk of mixing together uncoordinated Spring testing models
	// @InjectMocks
	// private DotnetConfigService dotnetConfigService;
	@MockBean
	private DotnetConfigService dotnetConfigService;
	private String route = null;
	private ResultActions resultActions;

	// @Disabled
	@DisplayName("The /config.js returns valid JS with the config object.")
	@Test
	void test1() throws Exception {
		route = "/api/uploads/config.js";
		Map<String, Object> config = new HashMap<>();
		config.put("TUS_ENDPOINT", "/api/uploads/tus");

		when(dotnetConfigService.buildConfig()).thenReturn(config);

		resultActions = mockMvc.perform(get(route));
		resultActions.andExpect(status().isOk())
				// MediaType.valueOf(
				.andExpect(content().contentTypeCompatibleWith("application/javascript"))
				.andExpect(content().string(containsString("window.APP_CONFIG = ")));
	}

	// @Disabled
	@DisplayName("The /config.js returns valid JS in case of server error.")
	@Test
	void test2() throws Exception {
		when(dotnetConfigService.buildConfig()).thenThrow(new RuntimeException("API Error"));
		route = "/api/uploads/config.js";
		resultActions = mockMvc.perform(get(route));
		resultActions.andExpect(status().isServiceUnavailable());
	}

	@DisplayName("The config endpoint returns JSON config")
	@Test
	void test4() throws Exception {

		Map<String, Object> config = new HashMap<>();
		config.put("TUS_ENDPOINT", "/api/uploads/tus");
		config.put("TUS_CHUNK_SIZE", 5242880);
		config.put("TUS_RETRY_DELAYS", List.of("0", "500", "1000"));

		when(dotnetConfigService.buildConfig()).thenReturn(config);

		mockMvc.perform(get("/api/uploads/config")).andExpect(status().isOk())
				.andExpect(content().contentTypeCompatibleWith("application/json"))
				.andExpect(jsonPath("$.TUS_ENDPOINT").value("/api/uploads/tus"))
				.andExpect(jsonPath("$.TUS_CHUNK_SIZE").value(5242880))
				.andExpect(jsonPath("$.TUS_RETRY_DELAYS[1]").value("500"));
	}
}
