package example.controller;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/")
public class AppController {

	// index page
	@GetMapping(produces = MediaType.TEXT_HTML_VALUE)
	@ResponseBody
	public ResponseEntity<String> index() {
		String body = "<html>" + "<head>" + "demo app" + "</head>" + "<body>"
				+ "<a href=\"./insert\"\"> Insert Dummy Metrics</a><br/>"
				+ "<a href=\"./query\"\">Dump metrics</a><br/>"
				+ "<a href=\"./list\"\">Server inventory</a><br/>"
				+ "</body>" + "</html>";
		return ResponseEntity.ok().body(body);
	}

}
