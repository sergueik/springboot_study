package example.service;

/**
 * Copyright 2023,2024 Serguei Kouzmine
 */

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.core.io.UrlResource;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;
import java.io.IOException;
import java.net.MalformedURLException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

// based on:
@Service
public class FileStorageService {

	private static final Logger logger = LoggerFactory.getLogger(FileStorageService.class);

	private final Path fileStorageLocation;

	protected String osName = null;

	// NOTE: NPE reading the value
	// @Value("${file.upload-dir-windows}")
	private String uploadDirWindows = "c:/temp/upload";
	// @Value("${file.upload-dir-linux}")
	private String uploadDirLinux = "/tmp/upload";

	private String uploadDir = null;

	public String getUploadDir() {
		return uploadDir;
	}

	public void setUploadDir(String value) {
		uploadDir = value;
	}

	@Autowired
	public FileStorageService() {
		// uploadDir = uploadDirWindows;
		if (uploadDir == null) {
			osName = getOSName();
			uploadDir = osName.equals("windows") ? uploadDirWindows : uploadDirLinux;
		}
		logger.info("UploadDir: " + uploadDir);

		this.fileStorageLocation = Paths.get(uploadDir).toAbsolutePath().normalize();

		try {
			Files.createDirectories(this.fileStorageLocation);
		} catch (Exception ex) {
			throw new RuntimeException("Could not create the upload directory.", ex);
		}
	}

	public String storeFile(MultipartFile file) {
		// Normalize file name
		String fileName = StringUtils.cleanPath(file.getOriginalFilename());

		try {
			// Check if the file's name contains invalid characters
			if (fileName.contains("..")) {
				throw new RuntimeException("Invalud Filename: " + fileName);
			}

			// Copy file to the target location (Replacing existing file with
			// the same
			// name)
			Path targetLocation = this.fileStorageLocation.resolve(fileName);
			Files.copy(file.getInputStream(), targetLocation, StandardCopyOption.REPLACE_EXISTING);

			return fileName;
		} catch (IOException ex) {
			throw new RuntimeException("Could not store file " + fileName + ". Please try again!", ex);
		}
	}

	public Resource loadFileAsResource(String fileName) {
		try {
			Path filePath = this.fileStorageLocation.resolve(fileName).normalize();
			Resource resource = new UrlResource(filePath.toUri());
			if (resource.exists()) {
				return resource;
			} else {
				throw new RuntimeException("File not found " + fileName);
			}
		} catch (MalformedURLException ex) {
			throw new RuntimeException("File not found: malformed url " + fileName, ex);
		}
	}

	// Utilities
	public String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

}
