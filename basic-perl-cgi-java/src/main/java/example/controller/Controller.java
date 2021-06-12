package example.controller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.service.ExampleService;

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

	@GetMapping(value = "/cgi-bin/{script:[a-z.0-9]+}", produces = MediaType.APPLICATION_JSON_VALUE)
	public String run(@PathVariable String script) {
		log.info(String.format("Running cgi-bin script: %s", script));
		return service.runScript(script);
	}

	@GetMapping(value = "/bad/cgi-bin/{script}", produces = MediaType.APPLICATION_JSON_VALUE)
	public String badRun(@PathVariable String script) {
		log.info(String.format("Running cgi-bin script: %s", script));
		return service.runScript(script);
	}

}
