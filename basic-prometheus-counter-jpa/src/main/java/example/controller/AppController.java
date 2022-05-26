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
		String body = "<html>" + "<head>" + "</head>" + "<body>"
				+ "<a href=\"./rawmetrics\"\">Application hosted metrics (untyped tag query)</a><br/>"
				+ "<a href=\"./typedmetrics\"\">Application hosted metrics (strongly typed tag query)</a><br/>"
				+ "<a href=\"./servers\"\">Server inventory DB query</a><br/>"
				+ "<a href=\"./data\"\"> Untyped Server inventory DB query</a><br/>"
				+ "</body>" + "</html>";
		return ResponseEntity.ok().body(body);
	}

}
