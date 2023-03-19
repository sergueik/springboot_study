package example.controller;
/**
 * Copyright 2022,2023 Serguei Kouzmine
 */

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;
import org.apache.commons.codec.binary.Base64;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;

@RestController
@RequestMapping("/")
public class Controller {
	private boolean debug = false;

	public void setDebug(boolean data) {
		debug = data;
	}

	private Log log = LogFactory.getLog(this.getClass());

	// @Autowired
	private ExampleService service;

	public Controller(ExampleService data) {
		service = data;
	}

	@PostMapping(value = "/cgi-bin/{script:status.cgi}", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public String statusBroken(@PathVariable String script,
			RequestEntity<String> request) {

		String body;
		try {
			// body = new String(Base64.decodeBase64(request.getBody()));
			// NOTE:
			// getBody() returns everything in one string:
			// call curl -H "Content-Type: appliction/json" -X POST -d '{"foo": 10,
			// "bar": 30}' http://192.168.99.100:8085/cgi-bin/status.cgi
			// "Content-Type: appliction/json=&{"foo": "bar"}="
			body = URLDecoder.decode(request.getBody(), "utf8");
			return service.runScript(script, body);
		} catch (UnsupportedEncodingException e) {
			return "";
		}

	}

	// try to have more specific annotation
	@PostMapping(value = "/cgi-bin/{script:status[0-9].cgi}", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public String status(@PathVariable String script, @RequestBody String body) {
		return service.runScript(script, body);
	}

	//
	// Free-hand argument will be possible to read through RequestEntity
	// https://stackoverflow.com/questions/52842979/how-to-get-request-url-in-spring-boot
	// The question mark does not have REGEX meaning in the mask
	@GetMapping(value = "/cgi-bin/{script:[a-z.0-9]+}", produces = MediaType.APPLICATION_JSON_VALUE)
	public String legacyParam(@PathVariable String script,
			RequestEntity<String> request) {
		String query = request.getUrl().getQuery();

		String[] commandlineArgs = (query == null) ? new String[] {}
				: query.split("\\s+");

		log.info(String.format("Running cgi-bin script: %s with args: %s", script,
				String.join(",", commandlineArgs)));
		return service.runScript(script, commandlineArgs);
	}

	@GetMapping(value = "/bad/cgi-bin/{script}", produces = MediaType.APPLICATION_JSON_VALUE)
	public String badRun(@PathVariable String script) {
		log.info(String.format("Running cgi-bin script: %s", script));
		return service.runScript(script);
	}

}
