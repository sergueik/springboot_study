package example.controller;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.util.Base64Utils;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpRequest;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.PropertySource;
import org.springframework.core.env.Environment;

import example.component.SearchRequest;
import example.component.SearchResponseRow;
import example.service.ExampleService;

// https://grafana.com/grafana/plugins/grafana-simple-json-datasource/
@RestController
@RequestMapping("/")
public class SearchController {

	@Autowired
	Environment environment;

	// @Autowired
	private ExampleService service;
	private static final Logger logger = LogManager.getLogger(SearchController.class);

	@Value("param")
	private String param;
	// private static final String param = capitalize("param");

	// for mocking
	public SearchController(ExampleService data) {
		service = data;
	}

	// array response
	@RequestMapping(method = RequestMethod.POST, value = "/search", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public List<String> postSearchRequest(@RequestBody SearchRequest data) {
		logger.info("postSearchRequest processing POST /search array response");
		List<String> result = new ArrayList<>();
		result.add("test");
		service.getDataMap("test");
		return result;
	}

	// not strongly typed legacy code
	@RequestMapping(value = "/search", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_VALUE)
	@ResponseBody
	public List<String> Search(HttpServletResponse response) {
		logger.info("processing POST /search");
		Utils.addResponseHeaders(response);
		List<String> result = new ArrayList<String>();
		result.add("data series");
		return result;
	}

	private static String capitalize(final String string) {
		if (string == null)
			throw new NullPointerException("null argument");
		if (string.equals(""))
			throw new NullPointerException("empty argument");

		return Character.toUpperCase(string.charAt(0)) + string.substring(1);
	}

	// map response
	@RequestMapping(method = RequestMethod.POST, value = "/search2", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<SearchResponseRow>> postSearch2Request(@RequestBody SearchRequest data) {

		logger.info("processing POST /search map response");
		List<SearchResponseRow> result = new ArrayList<>();
		SearchResponseRow row = new SearchResponseRow();
		row.setText("text data");
		row.setValue("value data");
		result.add(row);
		final HttpHeaders headers = Utils.addResponseHeaders();
		return ResponseEntity.status(HttpStatus.OK).headers(headers).contentType(MediaType.APPLICATION_JSON)
				.body(result);

	}

	// map response
	// NOTE: currently cannot distinguish
	@RequestMapping(method = RequestMethod.POST, value = "/search3", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<SearchResponseRow>> postSearch3Request(@RequestHeader Map<String, String> requestHeaders,
			@RequestBody SearchRequest data) {
		String value = null;
		logger.info("postSearch3Request processing POST /search map response");
		List<SearchResponseRow> result = new ArrayList<>();
		SearchResponseRow row = new SearchResponseRow();
		row.setText("text data");
		row.setValue("value data");
		result.add(row);

		logger.info("postSearch3Request looking for " + param);
		final HttpHeaders responseHeaders = Utils.addResponseHeaders();
		logger.info("postSearch3Request processing request headers" + requestHeaders.toString());
		if (requestHeaders.containsKey(capitalize(param))) {
			value = new String(Base64Utils.decodeFromString(requestHeaders.get(capitalize(param))));
			logger.info("postSearch3Request found special header: " + param + " = " + value);
			responseHeaders.add(param, Base64Utils.encodeToString(value.getBytes()));
		}
		service.getDataMap(value);
		return ResponseEntity.status(HttpStatus.OK).headers(responseHeaders).contentType(MediaType.APPLICATION_JSON)
				.body(result);

	}

	@RequestMapping(method = RequestMethod.POST, value = "/search4", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<List<SearchResponseRow>> postSearch4Request(HttpServletRequest request,
			@RequestBody SearchRequest data) {

		logger.info("postSearch4Request processing POST /search map response");
		List<SearchResponseRow> result = new ArrayList<>();
		SearchResponseRow row = new SearchResponseRow();
		row.setText("text data");
		row.setValue("value data");
		result.add(row);
		final HttpHeaders responseHeaders = Utils.addResponseHeaders();

		Enumeration<String> headerNames = request.getHeaderNames();
		logger.info("postSearch4Request looking for " + param);

		while (headerNames.hasMoreElements()) {
			String name = headerNames.nextElement();
			if (name.equalsIgnoreCase(param)) {
				String value = new String(Base64Utils.decodeFromString(request.getHeader(name)));
				logger.info("postSearch4Request adding response header: " + name + ":" + value);
				responseHeaders.add(name, Base64Utils.encodeToString(value.getBytes()));
			}
		}
		return ResponseEntity.status(HttpStatus.OK).headers(responseHeaders).contentType(MediaType.APPLICATION_JSON)
				.body(result);

	}

}
