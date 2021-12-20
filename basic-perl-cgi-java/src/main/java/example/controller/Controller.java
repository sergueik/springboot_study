package example.controller;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.MediaType;
import org.springframework.http.RequestEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
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
