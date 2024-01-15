package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Map;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/basic")
// see also: https://stackoverflow.com/questions/32184175/how-to-use-spring-redirect-if-controller-method-returns-responseentity
// https://stackoverflow.com/questions/6781396/spring-responseentity/15259403#15259403
public class RedirectWithParamController {

	private Map<String, String> data = new HashMap<>();

	@GetMapping(value = "/redirect1")
	public ResponseEntity<String> redirect1(@RequestParam String param) {
		return ResponseEntity.status(HttpStatus.FOUND)
				.header("Location", "confirm1?param=" + param).body(null);
	}

	@GetMapping(value = "/redirect2")
	public ResponseEntity<String> redirect2(@RequestParam String param) {
		return ResponseEntity.status(HttpStatus.FOUND)
				.header("Location", "confirm2/" + param).body(null);
	}

	@GetMapping(value = "/confirm1", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	@ResponseBody
	public Map<String, String> confirm1(@RequestParam String param) {
		data.put("param", param);
		return data;
	}

	@RequestMapping(method = RequestMethod.GET, value = "/confirm2/{param}", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	@ResponseBody
	public Map<String, String> confirm2(@PathVariable("param") String param) {
		data.put("param", param);
		return data;
	}

}
