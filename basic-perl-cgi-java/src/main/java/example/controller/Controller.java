package example.controller;
/**
 * Copyright 2022,2023 Serguei Kouzmine
 */

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
import java.nio.charset.StandardCharsets;

@RestController
@RequestMapping("/")
public class Controller {
	private boolean debug = false;

	public void setDebug(boolean data) {
		debug = data;
	}

	private Log log = LogFactory.getLog(this.getClass());

	@Autowired
	private ExampleService service;

	public Controller(ExampleService data) {
		service = data;
	}

	@PostMapping(value = "/cgi-bin/{script:[a-z.0-9]+.sh}", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public String shell(@PathVariable String script,
			RequestEntity<String> request) {
		String body = request.getBody();
		log.info("processing shell script: " + script);
		final String scriptDir = "/var/www/localhost/cgi-bin";
		return service.runProcess(String.format("%s/%s", scriptDir, script), body);
	}

	@PostMapping(value = "/cgi-bin/{script:status.cgi}", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public String status(@PathVariable String script, @RequestBody String body) {
		return service.runCGiBINScript(script, body);
	}

	@PostMapping(value = "/cgi-bin/{script:status[0-9].cgi}", produces = MediaType.APPLICATION_JSON_VALUE, consumes = MediaType.APPLICATION_JSON_VALUE)
	public String status(@PathVariable String script, @RequestBody byte[] bytes) {
		return service.runCGiBINScript(script,
				new String(bytes, StandardCharsets.UTF_8));
	}

	//
	// Free-hand argument will be possible to read through RequestEntity
	// https://stackoverflow.com/questions/52842979/how-to-get-request-url-in-spring-boot
	// The question mark does not have REGEX meaning in the mask
	@GetMapping(value = "/cgi-bin/{script:[a-z.0-9]+cgi}", produces = MediaType.APPLICATION_JSON_VALUE)
	public String legacyParam(@PathVariable String script,
			RequestEntity<String> request) {
		String query = request.getUrl().getQuery();

		String[] commandlineArgs = (query == null) ? new String[] {}
				: query.split("\\s+");
		final String separator = ",";
		// NOTE: comma-joined commandline arguments formatting is non-standard
		log.info(String.format("Running cgi-bin script: %s with args: %s", script,
				String.join(separator, commandlineArgs)));
		return service.runCGiBINScript(script, commandlineArgs);
	}

	@GetMapping(value = "/bad/cgi-bin/{script}", produces = MediaType.APPLICATION_JSON_VALUE)
	public String badRun(@PathVariable String script) {
		log.info(String.format("Running cgi-bin script: %s", script));
		return service.runCGiBINScript(script);
	}

}
