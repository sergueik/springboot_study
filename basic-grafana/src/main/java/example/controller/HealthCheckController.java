package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

// https://grafana.com/grafana/plugins/grafana-simple-json-datasource/

@RestController
@RequestMapping("/")
public class HealthCheckController {

	private static final Logger logger = LogManager
			.getLogger(HealthCheckController.class);

	@GetMapping
	@ResponseBody
	public ResponseEntity<Object> heathcheck() {
		logger.info("processing GET /");
		HttpHeaders headers = Utils.addResponseHeaders();
		return ResponseEntity.status(HttpStatus.OK).headers(headers).body(null);
	}

}
