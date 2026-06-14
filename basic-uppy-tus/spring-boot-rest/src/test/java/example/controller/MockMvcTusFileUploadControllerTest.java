package example.controller;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.web.SpringJUnitWebConfig;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.servlet.mvc.method.annotation.RequestMappingHandlerMapping;

import example.utils.TusStorageResolver;
import me.desair.tus.server.TusFileUploadService;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Arrays;
import java.util.Random;

@WebAppConfiguration
@SpringJUnitWebConfig
@ContextConfiguration(classes = { TusFileUploadController.class })
public class MockMvcTusFileUploadControllerTest {

	@Autowired
	private WebApplicationContext webApplicationContext;

	@MockBean
	private TusFileUploadService tusFileUploadService;

	@MockBean
	private TusStorageResolver tusStorageResolver;

	@Test
	public void test1() throws Exception {
		MockMvc mockMvc = MockMvcBuilders.webAppContextSetup(webApplicationContext).build();
		mockMvc.perform(post("/api/upload")).andExpect(status().isOk());
	}
/*
	@Autowired
	RequestMappingHandlerMapping mapping;

	@Test
	void test2() {
		mapping.getHandlerMethods().forEach((k, v) -> System.out.println(k));
	}
*/
	@Autowired
	private WebApplicationContext context;

	// show loaded classes
	@Test
	public void dumpBeans() {
		Arrays.stream(context.getBeanDefinitionNames()).sorted().forEach(System.out::println);
	}
}
