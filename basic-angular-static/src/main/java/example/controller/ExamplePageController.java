package example.controller;

/**
 * Copyright 2021 Serguei Kouzmine
 */
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;

@RequestMapping("/")
public class ExamplePageController {

	@GetMapping
	public String welcome() {
		// returns static page, no servlet nor jsp processing will take place
		return "index";
	}

}
