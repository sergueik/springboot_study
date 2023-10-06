package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.client.RestTemplate;

import com.google.gson.Gson;

import example.utils.Utils;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

@RestController
@RequestMapping("/configs")
public class Controller {

	@ResponseBody
	@GetMapping(value = "/{filename}/load", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> file(@PathVariable String filename,
			@RequestParam Optional<Long> newer) {
		Utils.getOSName();
		final String filePath = "c:\\temp" + "\\" + filename;
		return (newer.isPresent())
				? ResponseEntity.status(HttpStatus.OK)
						.body(Utils.getFileData(filePath, newer.get()))
				: ResponseEntity.status(HttpStatus.OK)
						.body(Utils.getFileData(filePath));

	}

	@ResponseBody
	@GetMapping(value = "/file_hash", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> fileHash(
			@RequestParam String filename, @RequestParam Optional<Long> newer,
			@RequestParam Optional<String> hash) {
		Utils.getOSName();
		// not doing real hash comparison
		final String filePath = "c:\\temp" + "\\" + filename;
		return (hash.isPresent())
				? ResponseEntity.status(HttpStatus.OK)
						.body(Utils.getErrorResponse("hash"))
				: (newer.isPresent())
						? ResponseEntity.status(HttpStatus.OK)
								.body(Utils.getFileData(filePath, newer.get()))
						: ResponseEntity.status(HttpStatus.OK)
								.body(Utils.getFileData(filePath));

	}

	@ResponseBody
	@GetMapping(value = "/file_hash_status", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, Object>> fileHashStatus(
			@RequestParam String filename, @RequestParam Optional<Long> newer,
			@RequestParam Optional<String> hash) {
		Utils.getOSName();
		final String filePath = "c:\\temp" + "\\" + filename;
		if (hash.isPresent()) {
			return ResponseEntity.status(HttpStatus.NOT_MODIFIED)
					.body(new HashMap<String, Object>());
		} else if (newer.isPresent()) {
			return ResponseEntity.status(HttpStatus.ALREADY_REPORTED)
					.body(Utils.getErrorResponse("newer: " + newer.get()));
		} else
			return ResponseEntity.status(HttpStatus.OK)
					.body(Utils.getFileData("c:\\temp" + "\\" + filename));

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
