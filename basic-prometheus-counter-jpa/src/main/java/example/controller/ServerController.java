package example.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.stream.Collectors;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.web.bind.annotation.RestController;

import example.model.Server;
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

		logger.info("Starting reporting server instance applications");
		List<ServerInstanceApplication> payload = dao
				.findAllServerInstanceApplications();

		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

	@ResponseBody
	@GetMapping(value = "server", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<Server>> server(@RequestParam String server) {

		logger.info("Starting reporting server " + server);
		List<Server> payload = dao.findServer(server);

		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

	@ResponseBody
	@GetMapping(value = "serversregexp", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<Server>> serversregexp(
			@RequestParam Optional<List<String>> keys) {
		if (keys.isPresent() && keys.get().size() > 0) {
			String serverNamesRegexp = String.format("(%s)",
					String.join("|", keys.get()));
			logger.info("Starting reporting servers matching expression: "
					+ serverNamesRegexp);
			// build servers on the fly
			List<Server> payload = dao.findServersNativeRegexp(serverNamesRegexp)
					.stream()
					.map(columns -> new Server((int) columns[0], (String) columns[1]))
					.collect(Collectors.toList());

			return (payload == null)
					? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
					: ResponseEntity.status(HttpStatus.OK).body(payload);
		} else {
			// NOTE: typed response
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED)
					.body(new ArrayList<Server>());
		}

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

