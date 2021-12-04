package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

// @Controller
@RestController
@RequestMapping(path = "/")
public class WelcomeController {
	private final Logger logger = LoggerFactory
			.getLogger(WelcomeController.class);

	// @ResponseBody

	@GetMapping(path = "/welcome")
	public String welcome() {
		logger.info("called");
		return "welcome";
	}

}
