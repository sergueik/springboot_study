package example.controller;

/**
 * Copyright 2023 Serguei Kouzmine
 */
import example.payload.UploadFileResponse;
import example.service.FileStorageService;
import example.property.FileStorageProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import javax.servlet.http.HttpServletRequest;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

// based on: https://github.com/callicoder/spring-boot-file-upload-download-rest-api-example

@RestController
public class FileUploadController {

	private static final Logger logger = LoggerFactory
			.getLogger(FileUploadController.class);

	@Autowired
	private FileStorageService fileStorageService;

	@PostMapping("/uploadFile")
	public UploadFileResponse uploadFile(
			@RequestParam("file") MultipartFile file) {
		if (file.getOriginalFilename().isEmpty())
			return new UploadFileResponse(null, null, null, 0);
		logger.info("upload file: " + file.getOriginalFilename());
		String fileName = fileStorageService.storeFile(file);

		String fileDownloadUri = ServletUriComponentsBuilder
				.fromCurrentContextPath().path("/downloadFile/").path(fileName)
				.toUriString();
		// NOTE: if the file parameter is empty the custom exception
		// example.exception.FileStorageException:
		// Could not store file . Please try again!
		// with root cause
		// java.nio.file.DirectoryNotEmptyException: "<the download directory>"
		// is raised
		return new UploadFileResponse(fileName, fileDownloadUri,
				file.getContentType(), file.getSize());
	}

	@PostMapping("/uploadMultipleFiles")
	public List<UploadFileResponse> uploadMultipleFiles(
			@RequestParam("files") MultipartFile[] files) {
		logger
				.info("upload " + files.length + " files: "
						+ Arrays.asList(files).stream()
								.map(file -> file.getOriginalFilename())
								.collect(Collectors.toList()));
		return Arrays.asList(files).stream().map(file -> uploadFile(file))
				.collect(Collectors.toList());
	}

	@GetMapping("/downloadFile/{fileName:.+}")
	public ResponseEntity<Resource> downloadFile(@PathVariable String fileName,
			HttpServletRequest request) {
		// Load file as Resource
		Resource resource = fileStorageService.loadFileAsResource(fileName);

		// Try to determine file's content type
		String contentType = null;
		try {
			contentType = request.getServletContext()
					.getMimeType(resource.getFile().getAbsolutePath());
		} catch (IOException e) {
			logger.info("Could not determine file type of " + fileName);
		}

		// Fallback to the default content type if type could not be determined
		if (contentType == null) {
			contentType = "application/octet-stream";
		}

		return ResponseEntity.ok()
				.contentType(MediaType.parseMediaType(contentType))
				.header(HttpHeaders.CONTENT_DISPOSITION,
						"attachment; filename=\"" + resource.getFilename() + "\"")
				.body(resource);
	}

}
