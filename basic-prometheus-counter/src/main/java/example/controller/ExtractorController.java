package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.io.File;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
/**
 */

import org.springframework.web.bind.annotation.RestController;

import org.ini4j.*;

/*
 *  @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */

@RestController
@RequestMapping("/")
public class ExtractorController {

	private static final Logger logger = LogManager
			.getLogger(ExtractorController.class);

	private static final boolean debug = false;

	// [org.springframework.http.converter.HttpMessageNotWitableException: No
	// converter for [class java.util.HashMap] with preset Content Type 'null']
	@ResponseBody
	@GetMapping(value = "extractors", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, String>> extractors() {

		final String section = "rpm";
		String tag = null;
		String name = null;
		String expression = null;
		if (debug)
			logger.info("process extractors for " + section);

		Map<String, String> payload = new HashMap<>();
		try {
			File iniFile = new File(String.format("%s/src/main/resources/%s",
					System.getProperty("user.dir"), "data.ini"));
			Wini ini = new Wini(iniFile);
			// int value = ini.get("section", "setting", int.class);
			// double value = ini.get("section", "setting", double.class);

			expression = ini.get(section, "expression");
			name = ini.get(section, "name");
			tag = ini.get(section, "tag");

			payload.put("name", name);
			logger.info("name: " + name);
			payload.put("tag", tag);
			logger.info("tag: " + tag);

			payload.put("expression", expression);
			logger.info("expression: " + expression);

			// To catch basically any error related to finding the file e.g
			// (The system cannot find the file specified)

		} catch (Exception e) {
			logger.info("Exception: " + e.toString());

		}
		return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON)
				.body(payload);
	}

}
