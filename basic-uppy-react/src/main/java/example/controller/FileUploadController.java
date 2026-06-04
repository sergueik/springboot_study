package example.controller;

import java.io.IOException;

/**
 * Copyright 2023,2024,2026 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import example.service.FileStorageService;
import example.utils.Utils;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
// based on: https://github.com/callicoder/spring-boot-file-upload-download-rest-api-example
import javax.servlet.http.Part;

@RestController
public class FileUploadController {

	private static final Logger logger = LoggerFactory.getLogger(FileUploadController.class);

	@Autowired
	private FileStorageService fileStorageService;

	@GetMapping("/listFiles")
	public String listFiles() {
		String listing = Utils.listDirecroryFiles(fileStorageService.getUploadDir());
		logger.info("Listing: {}", listing);
		return listing;
	}

	@PostMapping("/uploadFile")
	public String uploadFile(@RequestParam("file") MultipartFile file) {
		if (file.getOriginalFilename().isEmpty())
			// cannot currently upload zero size files
			return null;
		logger.info("upload file: " + file.getOriginalFilename());
		String fileName = fileStorageService.storeFile(file);

		// NOTE: when the file parameter is empty the custom exception is raised
		// example.exception.FileStorageException:
		// Could not store file . Please try again!
		// with root cause
		// java.nio.file.DirectoryNotEmptyException: "<the download directory>"
		return fileName;
	}

	// To log the number of chunks of a file received in a Spring Boot application,
	// it must capture the metadata sent by the client during a chunked upload
	// process.
	@RequestMapping(value = "/uploadMultipleFiles", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public Map<String, Object> uploadMultipleFiles(HttpServletRequest request,
			@RequestParam("files") MultipartFile[] files,
			@RequestParam(value = "chunk", required = false) Integer chunk,
			@RequestParam(value = "chunks", required = false) Integer totalChunks) {

		logger.info("Content-Type: {}", request.getContentType());
		logger.info("Content-Length: {}", request.getContentLengthLong());

		try {
			Collection<Part> parts = request.getParts();
			logger.info("Servlet parts count: {}", parts.size());

			for (Part part : parts) {
				logger.info("part name={} submittedFileName={} size={} contentType={}", part.getName(),
						part.getSubmittedFileName(), part.getSize(), part.getContentType());
			}
		} catch (IOException | ServletException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		logger.info("upload " + files.length + " files: "
				+ Arrays.asList(files).stream().map(file -> file.getOriginalFilename()).collect(Collectors.toList()));

		String filesList = Arrays.asList(files).stream().map(file -> file.getOriginalFilename())
				.collect(Collectors.joining(","));
		if (totalChunks != null) {
			logger.info("Receiving chunk {} of {} for files set: {}", chunk + 1, totalChunks, filesList);
		} else {
			logger.info("Received full payload (non-chunked): {}", filesList);
		}

		for (MultipartFile file : files) {
			logger.info("file={} size={}", file.getOriginalFilename(), file.getSize());
		}
		Arrays.asList(files).stream().forEach(file -> uploadFile(file));
		String listing = Utils.listDirecroryFiles(fileStorageService.getUploadDir(), false);
		logger.info("Listing: {}", listing);
		String delimiter = ",";
		Map<String, Object> response = new HashMap<>();
		response.put("success", true);
		response.put("uploaded", Arrays.asList(listing.split(delimiter)));
		response.put("url", "/upload-success");
		return response;
	}
}
