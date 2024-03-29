package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.io.IOException;
import java.io.InputStream;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;


import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.PathVariable;

import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.web.bind.annotation.RestController;

import example.dao.JDBCDao;
import example.service.NodeExporter;

/*
 *  @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */

@RestController
@RequestMapping("/")
public class NodeExporterController {

	private static final Logger logger = LogManager
			.getLogger(NodeExporterController.class);

	@Autowired
	private JDBCDao dao;

	@Autowired
	private NodeExporter nodeExporter;
	private static final boolean debug = false;

	// application hosted metrics
	// see also:
	// https://www.tabnine.com/code/java/methods/io.prometheus.client.CollectorRegistry/metricFamilySamples
	@ResponseBody
	@GetMapping(value = "metrics", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> metrics() {

		logger.info("Starting reporting metrics");
		String payload = nodeExporter.metrics();

		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

	protected String getScriptContent(String scriptName) {
		try {
			final InputStream stream = this.getClass().getClassLoader()
					.getResourceAsStream(scriptName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(scriptName);
		}
	}

	public static long dateToEpoch(String strDate) {
		String pattern = "MMM dd yyyy HH:mm:ss.SSS zzz";
		String precision = "millisecond";
		return dateToEpoch(strDate, pattern, precision);
	}

	public static long dateToEpoch(String strDate, String pattern) {
		String precision = "millisecond";
		return dateToEpoch(strDate, pattern, precision);
	}
	// java 8
	// based on:
	// https://stackoverflow.com/questions/6687433/convert-a-date-format-in-epoch
	// see also
	// https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html

	public static long dateToEpoch(String strDate, String pattern,
			String precision) {
		// String
		DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(pattern);
		ZonedDateTime zonedDateTime = ZonedDateTime.parse(strDate,
				dateTimeFormatter);
		Instant instant = zonedDateTime.toInstant();
		return precision.equals("second") ? instant.getEpochSecond()
				: instant.toEpochMilli();
	}

	// application hosted static file metrics - used for prometheus testing
	@ResponseBody
	@GetMapping(value = "instantmetrics", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> instantMetrics() {

		Instant timestamp = Instant.now();
		Date date = new Date(timestamp.getEpochSecond());
		String pattern = "EEE MMM d HH:mm:ss zzz yyyy";
		final ZoneId UTC_ZONE = ZoneId.of("UTC");
		DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(pattern);

		logger.info("Sending instant metrics for the current time: "
				+ timestamp.atZone(UTC_ZONE).format(dateTimeFormatter));
		// http://www.java2s.com/Tutorials/Java/Java_Format/0030__Java_Date_Format_Symbol.htm
		String payload = getScriptContent("instantmetrics.txt");
		long pastTimestamp = timestamp.toEpochMilli();
		// Instant.ofEpochSecond(0L).until(Instant.now(), ChronoUnit.MILLIS);

		String[] lines = payload.split("\r?\n");
		for (int cnt = 0; cnt != lines.length; cnt++) {
			if (!(lines[cnt].matches("^#.*"))) {
				lines[cnt] = String.format("%s %d", lines[cnt], pastTimestamp);
			}
		}
		payload = String.join("\n", lines);
		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}

	// application hosted static file metrics - used for prometheus testing
	@ResponseBody
	@GetMapping(value = "pastmetrics/{minute}", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> pastMetrics(@PathVariable String minute) {

		logger.info("Sending past metrics");

		String payload = getScriptContent("metrics.txt");
		
		Instant timestamp = Instant.now().minus(Integer.parseInt(minute ) , ChronoUnit.MINUTES);
		long pastTimestamp = timestamp.toEpochMilli() ; 
		// long pastTimestamp = timestamp.toEpochMilli() - 60 * Integer.parseInt(minute ) * 1000;
		// Instant.ofEpochSecond(0L).until(Instant.now(), ChronoUnit.MILLIS);
		
		String pattern = "EEE MMM d HH:mm:ss zzz yyyy";
		final ZoneId UTC_ZONE = ZoneId.of("UTC");
		DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(pattern);

		logger.info("Sending historic metrics for the past timestamp: "
				+ timestamp.atZone(UTC_ZONE).format(dateTimeFormatter));
		String[] lines = payload.split("\r?\n");
		for (int cnt = 0; cnt != lines.length; cnt++) {
			if (!(lines[cnt].matches("^#.*"))) {
				lines[cnt] = String.format("%s %d", lines[cnt], pastTimestamp);
			}
		}
		payload = String.join("\n", lines);
		return (payload == null)
				? ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null)
				: ResponseEntity.status(HttpStatus.OK).body(payload);
	}
}

