package example.controller;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

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
import example.repository.AxixsRepository;
import example.service.NodeExporter;

@RestController
@RequestMapping("/")
public class NodeExporterController {

	private static final Logger logger = LogManager
			.getLogger(NodeExporterController.class);

	@Autowired
	private NodeExporter nodeExporter;
	private static final boolean debug = false;

	// application hosted metrics
	// see also:
	// https://www.tabnine.com/code/java/methods/io.prometheus.client.CollectorRegistry/metricFamilySamples

	@ResponseBody
	@GetMapping(value = "rawmetrics", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> metricsFromData() {

		logger.info("Starting reporting raw metrics");
		// String payload = nodeExporter.metricsFromData();
		String payload = nodeExporter.metricsFromDataNative();
		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

	// http://localhost:8080/filteredrawmetrics?keys=hostname00,hostname01
	@ResponseBody
	@GetMapping(value = "filteredrawmetrics", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> metricsFromFilteredData(
			@RequestParam Optional<List<String>> keys) {
		if (keys.isPresent() && keys.get().size() > 0) {
			String serverNamesRegexp = String.format("(%s)",
					String.join("|", keys.get()));

			logger.info("Starting reporting raw metrics filtered by regexp: ",
					serverNamesRegexp);
			// String payload = nodeExporter.metricsFromData();
			String payload = nodeExporter
					.metricsFromFilteredDataNative(serverNamesRegexp);
			return (payload == null)
					? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
					: ResponseEntity.status(HttpStatus.OK).body(payload);
		} else {
			// NOTE: typed response
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).body("");
		}

	}

	@ResponseBody
	@GetMapping(value = "typedmetrics", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> metricsViaServerInstanceApplications() {

		logger.info("Starting reporting typed metrics");

		String payload = nodeExporter.metricsFromServerInstanceList();
		// String payload = nodeExporter.metricsFromServerInstanceListNative();
		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

}
