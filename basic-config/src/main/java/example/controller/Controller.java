package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.utils.Utils;

@RestController
@RequestMapping("/configs")
public class Controller {

	private static Gson gson = new GsonBuilder().create();
	private static String body = null;
	private String md5sum = null;
	private Map<String, Object> response;
	private String filePath;
	private HttpStatus status;
	private static final Logger logger = LoggerFactory
			.getLogger(Controller.class);

	@Value("${config.dir:c:\\TEMP}")
	private String configDir;

	@ResponseBody
	@GetMapping(value = "/{filename}/load", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> file(@PathVariable String filename,
			@RequestParam Optional<Long> newer) {
		Utils.getOSName();
		final String filePath = configDir + "\\" + filename;
		return (newer.isPresent())
				? ResponseEntity.status(HttpStatus.OK)
						.body(Utils.getFileData(filePath, newer.get()))
				: ResponseEntity.status(HttpStatus.OK)
						.body(Utils.getFileData(filePath));

	}

	// convert String to InputStream
	// http://www.java2s.com/example/java/java.io/convert-string-to-inputstream.html
	public static InputStream getInputStream(final String data) {
		return new ByteArrayInputStream(data.getBytes());
	}

	// NOTE: when MediaType is APPLICATION_JSON_VALUE
	// the client test receives:
	// {"inputStream":{"buf":[123,34,114,101,115,117,108,116,34,58,34,101,114,114,111,114,32,109,101,115,115,97,103,101,34,44,34,115,116,97,116,117,115,34,58,34,101,114,114,111,114,34,125],"pos":0,"mark":0,"count":43},
	// "description":"resource loaded through InputStream","read":false }
	@ResponseBody
	@GetMapping(value = "/file_hash", produces = {
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<InputStreamResource> fileHashx(
			@RequestParam String filename, @RequestParam Optional<Long> newer,
			@RequestParam Optional<String> hash) {
		response = Utils.getErrorResponse("newer: 12345");
		body = gson.toJson(response, Map.class);
		Utils.getOSName();
		filePath = configDir + "\\" + filename;
		boolean resourceStatus = false;

		if (hash.isPresent()) {
			// not doing any file hash comparison
			response = Utils.getErrorResponse("hash");
			body = gson.toJson(response, Map.class);
			md5sum = Utils.md5Sum(filePath);
			logger.info("md5Sum of {}: {}", filePath, md5sum);
			if (!md5sum.equalsIgnoreCase(hash.get()))
				resourceStatus = true;
		} else if (newer.isPresent()) {
			// not doing modification time stamp comparison
			response = Utils.getFileData(filePath, newer.get());
			body = gson.toJson(response, Map.class);
			resourceStatus = false;
		} else {
			resourceStatus = true;
		}
		if (resourceStatus) {
			try {
				body = new String(Files.readAllBytes(Paths.get(filePath)),
						StandardCharsets.UTF_8);
			} catch (IOException e) {
				body = "";
			}
		}

		return ResponseEntity.status(HttpStatus.OK)
				.body(new InputStreamResource(getInputStream(body)));
	}

	@ResponseBody
	@GetMapping(value = "/file_hash_status", produces = {
			MediaType.APPLICATION_OCTET_STREAM_VALUE })
	public ResponseEntity<InputStreamResource> fileHashStatus(
			@RequestParam String filename, @RequestParam Optional<Long> newer,
			@RequestParam Optional<String> hash) {
		Utils.getOSName();
		boolean resourceStatus = false;
		status = HttpStatus.OK;
		filePath = configDir + "\\" + filename;
		if (hash.isPresent()) {
			status = HttpStatus.NOT_MODIFIED;
			md5sum = Utils.md5Sum(filePath);
			logger.info("md5Sum of {}: {}", filePath, md5sum);
			body = "";
			if (!md5sum.equalsIgnoreCase(hash.get())) {
				status = HttpStatus.OK;
				resourceStatus = true;
			}
		} else if (newer.isPresent()) {
			status = HttpStatus.ALREADY_REPORTED;
			response = Utils.getErrorResponse("newer: " + newer.get());
			body = gson.toJson(response, Map.class);
			resourceStatus = false;
		} else {
			status = HttpStatus.OK;
			resourceStatus = true;
		}
		if (resourceStatus) {
			try {
				body = new String(Files.readAllBytes(Paths.get(filePath)),
						StandardCharsets.UTF_8);
			} catch (IOException e) {
				body = null;
			}
		}
		return ResponseEntity.status(status)
				.body(new InputStreamResource(getInputStream(body)));
	}

	@ResponseBody
	@GetMapping(value = "/{hostname}/list", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	// copy NUL c:\temp\a.conf
	// copy NUL c:\temp\b.conf
	// copy NUL c:\temp\base\c.conf
	// GET http://localhost:8085/basic/file
	// {"a":1690604332,"b":1690604335,"base:c":1690604632}
	// GET http://localhost:8085/basic/file?newer=1690604333
	// {"b":1690604335,"base:c":1690604632}
	public ResponseEntity<Map<String, Long>> list(@PathVariable String hostname,
			@RequestParam Optional<Long> newer) {
		Utils.getOSName();
		try {
			return (newer.isPresent())
					? ResponseEntity.status(HttpStatus.OK)
							.body(Utils.listFileData("c:\\temp", newer.get()))
					: ResponseEntity.status(HttpStatus.OK)
							.body(Utils.listFileData("c:\\temp"));
		} catch (IOException e) {
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED)
					.body(new HashMap<String, Long>());
		}
	}

}
