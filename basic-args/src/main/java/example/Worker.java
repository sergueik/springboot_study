package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@Component
@RestController
@RequestMapping("/basic")
public class Worker {

	@Autowired
	// not exposed about that params is linked to ApplicationArguments
	private Params params;

	private static final Logger logger = LoggerFactory.getLogger(Worker.class);

	public void logConfiguration() {
		logger.info("Loaded with params: " + params.getId());
	}

	@GetMapping
	public String Process() {
		final String appname = params.getAppname();
		final int result = params.getResult();
		return "This is " + appname + " and the result is: " + result;
	}
}
