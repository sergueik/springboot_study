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
	public ResponseEntity<String> frozenMetrics() {

		Instant timestamp = Instant.now();
		Date date = new Date(timestamp.getEpochSecond());
		String pattern = "EEE MMM d HH:mm:ss zzz yyyy";
		final ZoneId UTC_ZONE = ZoneId.of("UTC");
		DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(pattern);

		logger.info("Sending instant metrics for the current time: "
				+ timestamp.atZone(UTC_ZONE).format(dateTimeFormatter));
		// http://www.java2s.com/Tutorials/Java/Java_Format/0030__Java_Date_Format_Symbol.htm
		String payload = getScriptContent("metrics.txt");
		// date -d "-1 day"
		// NOTE: fragile - prome to
		// Exception in thread "main" java.time.format.DateTimeParseException:
		// Text 'Fri Jul 21 13:52:59 EDT 2022' could not be parsed at index 8
		// d does not fail with two digit date
		// d or dd?
		// dd fails with single digit

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
	@GetMapping(value = "staticmetrics", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> staticMetrics() {

		logger.info("Sending static metrics");

		String payload = getScriptContent("metrics.txt");
		String strDate = "Thu Jul 7 16:30:21 EDT 2022";
		                  
		// add data points in the past
		// date -d "-1 day"
		// NOTE: fragile - prome to
		// Exception in thread "main" java.time.format.DateTimeParseException:
		// Text 'Fri Jul 21 13:52:59 EDT 2022' could not be parsed at index 8
		// d does not fail with two digit date
		// d or dd?
		// dd fails with single digit
		String pattern = "EEE MMM d HH:mm:ss zzz yyyy";
		long pastTimestamp = dateToEpoch(strDate, pattern);
		
		Instant timestamp = Instant.now();
		Date date = new Date(timestamp.getEpochSecond());
		pastTimestamp = timestamp.toEpochMilli() - 60 * 10 * 1000;
		// pastTimestamp = timestamp.toEpochMilli();
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
}

