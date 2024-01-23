package example.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

/**
 * Copyright 2023,2024 Serguei Kouzmine
 */

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.StringUtils;
import org.springframework.web.multipart.MultipartFile;

// based on: https://github.com/callicoder/spring-boot-file-upload-download-rest-api-example/blob/master/src/main/java/com/example/filedemo/controller/FileController.java#L44
@Service
public class FileStorageService {

	private static final Logger logger = LoggerFactory.getLogger(FileStorageService.class);

	private Path fileStorageLocation;

	protected String osName = null;

	@Value("${file.upload-dir-windows}")
	private String uploadDirWindows;
	@Value("${file.upload-dir-linux}")
	private String uploadDirLinux;

	private String uploadDir = null;

	public String getUploadDir() {

		if (uploadDir == null) {
			osName = getOSName();
			uploadDir = osName.equals("windows") ? uploadDirWindows : uploadDirLinux;
		}
		logger.info("UploadDir: " + uploadDir);
		return uploadDir;
	}

	public void setUploadDir(String value) {
		uploadDir = value;
	}

	public FileStorageService() {
		osName = getOSName();
	}

	public String storeFile(MultipartFile file) {

		if (uploadDir == null) {
			osName = getOSName();
			uploadDir = osName.equals("windows") ? uploadDirWindows : uploadDirLinux;
			logger.info("UploadDir defined: " + uploadDir);
		}
		// Normalize file name
		String fileName = StringUtils.cleanPath(file.getOriginalFilename());

		try {
			// Check if the file's name contains invalid characters
			if (fileName.contains("..")) {
				throw new RuntimeException("Invalud Filename: " + fileName);
			}

			// Copy file to target location, replacing existing
			if (fileStorageLocation == null) {
				fileStorageLocation = Paths.get(uploadDir).toAbsolutePath().normalize();

				try {
					Files.createDirectories(this.fileStorageLocation);
				} catch (Exception ex) {
					throw new RuntimeException("Could not create the upload directory.", ex);
				}
			}
			Path targetLocation = fileStorageLocation.resolve(fileName);
			Files.copy(file.getInputStream(), targetLocation, StandardCopyOption.REPLACE_EXISTING);

			return fileName;
		} catch (IOException ex) {
			throw new RuntimeException("Could not store file " + fileName + ". Please try again!", ex);
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
