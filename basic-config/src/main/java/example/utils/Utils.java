package example.utils;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.net.URL;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import java.nio.file.attribute.BasicFileAttributes;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Utils {

	
	protected static String osName;
	private static BasicFileAttributes basicFileAttributes;
	private static final Logger logger = LoggerFactory.getLogger(Utils.class);
	private static final String extensionMask = "\\.conf$";
	private static final String fileMask = ".*" + extensionMask;

	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

	public static Map<String, Object> getErrorResponse() {
		return getErrorResponse("error message");
	}

	public static Map<String, Object> getErrorResponse(String message) {
		// return getFileData(path);
		Map<String, Object> data = new HashMap<>();
		data.put("status", "error");
		data.put("result", message);
		return data;

	}

	public static Map<String, Object> getFileData(String dataFilePath) {
		Map<String, Object> data = new HashMap<>();
		data.put("status", "ok");
		data.put("result", new HashMap<>());
		return data;
	}

	public static Map<String, Object> getFileData(String path, long newer) {
		// return getFileData(path);
		Map<String, Object> data = new HashMap<>();
		data.put("status", "error");
		data.put("result", "error message");
		return data;

	}

	private static String getDataFileUri(String dataFilePath) {
		return osName.equals("windows")
				? "file:///" + dataFilePath.replaceAll("\\\\", "/")
				: "file://" + dataFilePath;
	}

	// based on:
	// http://www.java2s.com/example/java/file-path-io/get-file-last-modified-time.html
	public static long getLastModifiedEpochSeconds(File file) {
		if (file == null)
			return -1;

		try {
			basicFileAttributes = Files.readAttributes(file.toPath(),
					BasicFileAttributes.class);
			return basicFileAttributes.lastModifiedTime().toMillis() / 1000;
		} catch (IOException e) {
			logger.info("Exception (ignored) " + e.toString());
			return -1;
		}
	}

	public static Map<String, Long> listFileData(String path) throws IOException {
		return listFileData(path, -1);
	}

	// based on:
	// https://github.com/mkyong/core-java/blob/master/java-io/src/main/java/com/mkyong/io/api/FilesWalkExample.java
	public static Map<String, Long> listFileData(String path, long newer)
			throws IOException {
		final Map<String, Long> fileData = new HashMap<>();
		logger.info("Building path: {}", path);
		Path basePath = Paths.get(path);
		// NOTE: dealing with URLs - should not use
		// System.getProperty("file.separator")
		final String basePathUri = new URL(
				getDataFileUri(basePath.toAbsolutePath().toString())).getFile() + "/";
		logger.info("Scanning path: {}", basePathUri);
		List<Path> result;
		try (Stream<Path> walk = Files.walk(basePath)) {
			result = walk.filter(Files::isRegularFile)
					.filter(o -> o.getFileName().toString().matches(fileMask))
					.collect(Collectors.toList());
		}
		result.stream().forEach(o -> {
			try {

				String absolutePath = o.toAbsolutePath().toString();
				URL url = new URL(getDataFileUri(absolutePath));
				final String key = url.getFile().replaceFirst(basePathUri, "")
						.replaceAll("/", ":").replaceFirst(extensionMask, "");
				long lastModifiedEpochSeconds = getLastModifiedEpochSeconds(
						new File(absolutePath));
				logger.info("Examine info for: {}", absolutePath);
				if (lastModifiedEpochSeconds > newer) {
					logger.info("Collect File: {}", key);
					// Collect File: base:c
					fileData.put(key, lastModifiedEpochSeconds);
				} else {
					// TODO: calendar
					logger.info("File too old {}: {}", key, lastModifiedEpochSeconds);
				}
			} catch (IllegalArgumentException | IOException e) {
				logger.info("Exception (ignored): {}", e.toString());
			}
		});
		logger.info("returning: " + fileData.keySet());
		return fileData;

	}

}
