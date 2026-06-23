package example.controller;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.api.extension.ExtendWith;

import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.mockito.Mockito;
import org.mockito.Spy;
import org.mockito.junit.MockitoJUnitRunner;
import org.mockito.MockedStatic;

import static org.mockito.Mockito.when;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.nullValue;
import static org.hamcrest.Matchers.is;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import example.dto.FinalizeRequest;
import example.utils.TusStorageResolver;
import me.desair.tus.server.TusFileUploadService;
import me.desair.tus.server.exception.TusException;
import me.desair.tus.server.upload.UploadInfo;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8086" })

@ExtendWith(MockitoExtension.class)
public class FinalizeControllerTest {

	@Autowired
	@InjectMocks
	FinalizeController controller;

	@Mock
	TusFileUploadService tusFileUploadService;

	@Mock
	TusStorageResolver tusStorageResolver;

	private UploadInfo info = null;

	@BeforeEach
	public void setUp() throws IOException, TusException {

		info = Mockito.mock(UploadInfo.class);

		when(info.getFileName()).thenReturn("foo.bin");

		when(tusFileUploadService.getUploadInfo("/api/upload/123")).thenReturn(info);

		when(tusStorageResolver.resolve(info)).thenReturn(Files.createTempFile("test", ".bin"));
	}

	@DisplayName("This server does confirm upload is finished")
	@Test
	void test1() throws Exception {
		ResponseEntity<?> response = controller.uploadJson(new FinalizeRequest("123"));
		when(info.isUploadInProgress()).thenReturn(false);
		assertThat(response.getStatusCode(), is(HttpStatus.OK));
	}

	@DisplayName("This server does report upload in progress")
	@Test
	void test2() throws Exception {
		ResponseEntity<?> response = controller.uploadJson(new FinalizeRequest("123"));
		when(info.isUploadInProgress()).thenReturn(true);
		assertThat(response.getStatusCode(), is(HttpStatus.ACCEPTED));
	}
}