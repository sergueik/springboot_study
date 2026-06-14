package example.controller;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.web.SpringJUnitWebConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

import example.utils.TusStorageResolver;
import me.desair.tus.server.TusFileUploadService;


@WebMvcTest(TusFileUploadController.class)
class MockMvcTusFileUploadController1Test {

	@Autowired
	private MockMvc mockMvc;

	@MockBean
	private TusFileUploadService tusFileUploadService;

	@MockBean
	private TusStorageResolver tusStorageResolver;

	@Test
	void test1() throws Exception {
		mockMvc.perform(post("/api/upload")).andDo(print()).andExpect(status().isOk());
	}
}