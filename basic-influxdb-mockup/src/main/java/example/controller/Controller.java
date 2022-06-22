package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import examle.model.Point;
import example.utils.LineProtocolParser;

@RestController
@RequestMapping("/")
public class Controller {

	private final Logger logger = LoggerFactory.getLogger(Controller.class);

	private static final LineProtocolParser utils = LineProtocolParser
			.getInstance();
	private static String stringResult = null;
	private static Point pointResult = null;

	@RequestMapping(value = "/ping", method = RequestMethod.HEAD)
	public ResponseEntity<Void> ping() {
		return ResponseEntity.noContent().header("X-Influxdb-version", "OSS")
				.build();
	}

	@ResponseBody
	// 406 Not Acceptable client error response
	// 415 Unsupported Media Type
	@PostMapping(value = "write", /* consumes = MediaType.TEXT_PLAIN_VALUE, */ produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<Void> write(@RequestParam String db,
			@RequestBody String payload) throws UnsupportedEncodingException {
		logger.info("body: " + payload);
		String input = URLDecoder.decode(payload.replaceFirst("db=" + db + "&", ""),
				"UTF-8");
		logger.info("input: " + input);
		stringResult = utils.parseLineProtocolLine(input);
		logger.info("stringResult: " + stringResult);
		return ResponseEntity.noContent().header("result", stringResult).build();

	}

	// NOTE: temporarily keep both
	@PostMapping(value = "write2", /* consumes = MediaType.TEXT_PLAIN_VALUE, */ produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Point> write2(@RequestParam String db,
			@RequestParam String precision, @RequestBody String payload)
			throws UnsupportedEncodingException {
		logger.info("body: " + payload);

		String input = URLDecoder.decode(payload.replaceAll(
				"(?:" + "db=" + db + "|" + "precision=" + precision + ")" + "&", ""),
				"UTF-8");
		logger.info("input: " + input);
		pointResult = utils.extractPointFromLineProtocolLine(input);
		logger.info("pointResult: " + pointResult);
		// to test Ethe global xception Handler,uncomment the following and comment
		// last lines
		// throw new UnsupportedEncodingException();
		return ResponseEntity.status(HttpStatus.OK).body(pointResult);

	}

	@ExceptionHandler({ UnsupportedEncodingException.class })
	public ResponseEntity<String> handleException(Exception e) {
		return ResponseEntity.status(HttpStatus.BAD_REQUEST)
				.body("there was error: " + e.toString());

	}
}
