package example.controller;

/**
 * Copyright 2024 Serguei Kouzmine
 */
import static org.junit.Assert.assertEquals;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.nio.file.Files;

// import org.junit.jupiter.api.Test;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import org.springframework.web.multipart.MultipartFile;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.multipart;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.util.Random;

@WebAppConfiguration
@ContextConfiguration(classes = { Controller.class })
@RunWith(SpringJUnit4ClassRunner.class)
// based on:
// https://github.com/eugenp/tutorials/blob/master/spring-web-modules/spring-mvc-java-2/src/test/java/com/baeldung/multiparttesting/MultipartPostRequestControllerUnitTest.java
// see also
// https://github.com/eugenp/tutorials/blob/master/spring-web-modules/spring-mvc-java-2/src/test/java/com/baeldung/multipart/file/ConvertFileToMultipartFileUnitTest.java
// https://github.com/eugenp/tutorials/blob/master/spring-web-modules/spring-mvc-java-2/src/test/java/com/baeldung/multipart/file/ConvertMultipartFileUnitTest.java
// with real files
public class MockMultipartFileTest {

	@Autowired
	private WebApplicationContext webApplicationContext;
	private String contents = null;
	@Test
	public void test1() throws Exception {
		int size = 80;
		contents = getRandomString(size);

		MockMultipartFile file = new MockMultipartFile("file", "hello.txt",
				MediaType.TEXT_PLAIN_VALUE, contents.getBytes());

		MockMvc mockMvc = MockMvcBuilders.webAppContextSetup(webApplicationContext)
				.build();
		mockMvc.perform(multipart("/basic/simpleupload").file(file))
				.andExpect(status().isOk());
		// java.lang.IllegalArgumentException: WebApplicationContext is required
	}

	@Test
	public void test2() throws Exception {
		int size = 101 * 1024;
		contents = getRandomString(size);
		System.err
				.println(String.format("uploading %d bytes file", contents.length()));
		MockMultipartFile file = new MockMultipartFile("file", "hello.txt",
				MediaType.TEXT_PLAIN_VALUE, contents.getBytes());

		MockMvc mockMvc = MockMvcBuilders.webAppContextSetup(webApplicationContext)
				.build();
		mockMvc.perform(multipart("/basic/simpleupload").file(file))
				.andExpect(status().isOk());
		// java.lang.IllegalArgumentException: WebApplicationContext is required
	}

	// origin:
	// http://www.java2s.com/example/java-utility-method/random-string/getrandomstring-int-size-4c2ae.html
	public static String getRandomString(int size) {
		String data;

		char[] chars = "abcdefghijklmnopqrstuvwxyz".toCharArray();
		StringBuilder stringBuilder = new StringBuilder();
		Random random = new Random();
		for (int i = 0; i < size; i++) {
			char c = chars[random.nextInt(chars.length)];
			stringBuilder.append(c);
		}
		data = stringBuilder.toString();

		return data;
	}
}
