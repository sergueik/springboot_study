package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.Iterator;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.component.DataComponent;
import example.model.Data;

@RestController
@RequestMapping("/basic")
public class ExampleController {

	@Autowired
	DataComponent data;

	private static Gson gson = new GsonBuilder().create();
	private boolean debug = false;
	// see also about writing SpringBoot application tests without relying on
	// SpringBoot field injection
	// https://reflectoring.io/unit-testing-spring-boot/

	public ExampleController() {

	}

	@GetMapping
	public String hello() {
		ConcurrentHashMap.KeySetView<Integer, String> keySetView = data.keySet();
		Iterator<Integer> iterator = keySetView.iterator();

		long time = System.currentTimeMillis();
		String output = time + ": ";

		while (iterator.hasNext()) {
			Integer key = iterator.next();
			String value = data.getOrDefault(key, "");
			output += key + "=>" + value + "; ";
		}

		return output;

		// return service.hello();
	}

	@GetMapping(value = "/json", produces = { MediaType.APPLICATION_JSON_VALUE })
	public Data json() {
		return new Data("dummy");
	}

}
