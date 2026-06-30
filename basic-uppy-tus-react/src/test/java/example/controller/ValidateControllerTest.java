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

import static org.mockito.Mockito.when;
import static org.mockito.Mockito.verify;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.verifyNoMoreInteractions;


import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.io.IOException;

import example.dto.ValidateRequest;
import example.service.DigestService;
import example.utils.TusStorageResolver;
import me.desair.tus.server.TusFileUploadService;
import me.desair.tus.server.exception.TusException;
import me.desair.tus.server.upload.UploadInfo;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8086" })

@ExtendWith(MockitoExtension.class)
public class ValidateControllerTest {

    @InjectMocks
    private ValidateController controller;

	@Mock
	TusStorageResolver tusStorageResolver;

	@Mock
	private TusFileUploadService tusFileUploadService;

	@Mock
	private DigestService digestService;

	@Mock
	private UploadInfo uploadInfo;
	private ValidateRequest request = null;

	// arrange
	@BeforeEach
	public void setUp() throws IOException, TusException {
		request = new ValidateRequest();
		request.setUploadId("123");
		request.setHash("ABCDEF");

	}

	@Test
	void shouldDeleteAndCleanupAfterSuccessfulValidation() throws Exception {

		when(tusFileUploadService.getUploadInfo("/api/upload/123")).thenReturn(uploadInfo);

		when(uploadInfo.getFileName()).thenReturn("example.bin");

		when(digestService.digest(anyString())).thenReturn("ABCDEF");

		// act
		ResponseEntity<?> response = controller.uploadJson(request);

		// assert
		assertThat(response.getStatusCode(), is(HttpStatus.OK));

		verify(tusFileUploadService).deleteUpload("/api/upload/123");

		verify(tusFileUploadService).cleanup();

		verifyNoMoreInteractions(tusFileUploadService);
	}

}