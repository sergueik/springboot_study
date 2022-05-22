package example.controller;

import java.util.List;

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
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.web.bind.annotation.RestController;

import example.projection.ServerInstanceApplication;
import example.repository.AxixsRepository;
import example.service.NodeExporter;

@RestController
@RequestMapping("/")
public class ServerController {

	private static final Logger logger = LogManager
			.getLogger(ServerController.class);

	@Autowired
	private AxixsRepository dao;

	@ResponseBody
	@GetMapping(value = "servers", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<ServerInstanceApplication>> servers() {

		logger.info("Starting reporting metrics");
		List<ServerInstanceApplication> payload = dao
				.findAllServerInstanceApplications();

		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

	@ResponseBody
	@GetMapping(value = "data", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<Object[]>> data() {

		logger.info("Starting reporting metrics");
		List<Object[]> payload = dao.findAllData();

		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

}
