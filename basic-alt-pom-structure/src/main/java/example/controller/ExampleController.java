package example.controller;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Map;

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;


@RestController
@RequestMapping("/basic")
public class ExampleController {

	private static Gson gson = new GsonBuilder().create();
	private boolean debug = false;

	@GetMapping(value = "/json", produces = { MediaType.APPLICATION_JSON_VALUE })
	@ResponseBody
	public Map<String, String> quoteinjson() {
		Map<String, String> config = new HashMap<>();
		String name = "\"quoted\"";
		config.put("name", name);
		return config;
	}

}
