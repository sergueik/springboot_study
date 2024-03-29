package example.controller;
/**
 * Copyright 2023-2024 Serguei Kouzmine
 */

import java.util.Arrays;
import java.util.stream.Collectors;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import example.service.FileStorageService;
import example.utils.Utils;

// based on: https://github.com/callicoder/spring-boot-file-upload-download-rest-api-example

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

	@PostMapping("/uploadMultipleFiles")
	public String uploadMultipleFiles(@RequestParam("files") MultipartFile[] files) {
		logger.info("upload " + files.length + " files: "
				+ Arrays.asList(files).stream().map(file -> file.getOriginalFilename()).collect(Collectors.toList()));
		// no longer showing the uploaded files urls
		Arrays.asList(files).stream().forEach(file -> uploadFile(file));
		String listing = Utils.listDirecroryFiles(fileStorageService.getUploadDir());
		logger.info("Listing: {}", listing);
		return listing;
	}
}
