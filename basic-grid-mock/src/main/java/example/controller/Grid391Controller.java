package example.controller;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.model.grid391.Build;
import example.model.grid391.Data;
import example.model.grid391.Value;
import example.utils.Utils;

@RestController
@RequestMapping("/grid391")
public class Grid391Controller {

	private boolean debug = false;
	private final String version = "3.9.1";
	private static final Gson gson = new GsonBuilder().setPrettyPrinting()
			.create();

	@GetMapping(value = "/status", produces = MediaType.APPLICATION_JSON_VALUE)
	public Data json() {
		final String result = Utils.getScriptContent("grid391.json");
		Data data = gson.fromJson(result, Data.class);
		Value value = data.getValue();
		Build build = value.getBuild();

		build.setVersion(version);
		value.setBuild(build);
		data.setValue(value);
		return data;
	}

}
