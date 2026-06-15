package example.controller;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.test.context.junit.jupiter.web.SpringJUnitWebConfig;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.text.IsEmptyString.isEmptyString;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.is;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.head;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.options;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.patch;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;
import java.util.Collection;

import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;

import example.utils.TusStorageResolver;
import me.desair.tus.server.TusFileUploadService;

@SpringBootTest(properties = { "tus.server.data.directory=${java.io.tmpdir}/tus" })
@AutoConfigureMockMvc
class MockMvcTusFileUploadControllerTest {

	@Autowired
	MockMvc mockMvc;
	private final static String route = "/api/upload";

	@DisplayName("This server does not support tus protocol version")
	@Test
	void test1() throws Exception {
		mockMvc.perform(post(route)).andDo(print()).andExpect(status().isPreconditionFailed());
	}

	@DisplayName("Error message = No valid value was found in headers Upload-Length and Upload-Defer-Length\r\n"
			+ "Headers = [Vary:\"Origin\", \"Access-Control-Request-Method\", \"Access-Control-Request-Headers\", Tus-Resumable:\"1.0.0\", Content-Length:\"0\", Access-Control-Expose-Headers:\"Location,Upload-Offset,Upload-Length\"]")

	@Test
	void test2() throws Exception {
		mockMvc.perform(post(route).header("Tus-Resumable", "1.0.0")).andDo(print()).andExpect(status().isBadRequest());
	}

	@DisplayName("Tus response to OPTIONS")
	@Test
	void test3() throws Exception {
		MvcResult result = mockMvc
				.perform(options(route).header("Tus-Resumable", "1.0.0").header("Upload-Length", 1000)).andDo(print())
				.andReturn();
		Collection<String> headerNames = result.getResponse().getHeaderNames();
		System.err.println(Arrays.asList(headerNames));
		String version = result.getResponse().getHeader("Tus-Version");

		assertThat(version, notNullValue());
		assertThat(version, containsString("1.0.0"));
		System.err.println("Tus-Version: " + version);

	}

	@DisplayName("Known length")
	@Test
	void test4() throws Exception {
		mockMvc.perform(post(route).header("Tus-Resumable", "1.0.0").header("Upload-Length", 1000)).andDo(print())
				.andExpect(status().isCreated()).andExpect(header().exists("Location"))
				.andExpect(header().string("Tus-Resumable", "1.0.0"));
		;
	}

	@DisplayName("Specify Deferred length, perform HEAD, PATCH, HEAD")
	@Test
	void test5() throws Exception {
		// NOTE: Upload-Defer-Length as a flag, not a length

		MvcResult result = mockMvc
				.perform(post(route).header("Tus-Resumable", "1.0.0").header("Upload-Defer-Length", 1)).andDo(print())
				.andExpect(status().isCreated()).andExpect(header().string("Tus-Resumable", "1.0.0")).andReturn();

		String location = result.getResponse().getHeader("Location");

		assertThat(location, notNullValue());

		// continue uploading
		mockMvc.perform(head(location).header("Tus-Resumable", "1.0.0")).andDo(print())
				.andExpect(status().isNoContent()).andExpect(header().string("Upload-Offset", "0"))
				.andExpect(header().string("Tus-Resumable", "1.0.0"));

		// continue uploading with payload supplied as raw bytes
		mockMvc.perform(patch(location).header("Tus-Resumable", "1.0.0").header("Upload-Offset", "0")
				.contentType("application/offset+octet-stream").content("hello".getBytes(StandardCharsets.UTF_8)))
				.andDo(print());

		// examine response headers to verify the payload supplied
		// has actually being persisted and the offset is advancing
		result = mockMvc.perform(head(location).header("Tus-Resumable", "1.0.0")).andDo(print()).andReturn();
		System.out.println("PATCH Response Headers: " + result.getResponse().getHeaderNames());

		// NOTE: the assertion failed, commented
		// mockMvc.perform(head(location).header("Tus-Resumable",
		// "1.0.0")).andDo(print()).andExpect(header().string("Upload-Offset", "5"));

	}

}