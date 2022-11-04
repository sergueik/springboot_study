package example.controller;
/**
 * Copyright 2021,2022 Serguei Kouzmine
 */

import java.io.IOException;
import java.net.URISyntaxException;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.service.Grid3Service;
import example.service.Grid4Service;

@RestController
@RequestMapping("/status")
public class ExampleController {

	@Autowired
	private Grid4Service grid4;
	@Autowired
	private Grid3Service grid3;

	@RequestMapping(method = RequestMethod.GET, produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<?> status(
			@RequestParam(defaultValue = "") String version) {
		try {
			if (version.matches("3"))
				return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON)
						.body(grid3.status());
			else
				return ResponseEntity.ok().contentType(MediaType.APPLICATION_JSON)
						.body(grid4.status());
		} catch (NullPointerException e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
		}
	}

}
