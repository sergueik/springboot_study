package example.controller;

import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/task-scheduler")
public class Controller {

	private static final Logger logger = LogManager.getLogger(Controller.class);

	@GetMapping(value = "/healthcheck", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> hello() {
		logger.log(Level.INFO, "respond to healthcheck");
		return ResponseEntity.ok().build();
	}
}
