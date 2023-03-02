package io.sayadi.elasticapmspringbootintegration;

import org.springframework.context.annotation.Profile;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class TestController {

	@GetMapping("/super-fast")
	public String getSuperFastApi() {

		return "I'm super fast.";
	}

	@GetMapping("/fast")
	public String getFastApi() throws InterruptedException {

		Thread.sleep(20); // sleep for 20 milliseconds
		return "I'm fast!";
	}

	@GetMapping("/slow")
	public String getSlowApi() throws InterruptedException {

		Thread.sleep(3000); // sleep for 3 seconds
		return "I'm slow :(";
	}

	@GetMapping("/super-slow")
	@Profile({ "predev", "dev", "staging" })
	public String getSuperSlowApi() throws InterruptedException {

		Thread.sleep(60000); // sleep for 1 minute!
		return "I'm super slow. Refactor me before moving to production!! :)";
	}
}
