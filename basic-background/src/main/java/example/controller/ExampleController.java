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
import example.domain.Gender;
import example.domain.User;

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
		ConcurrentHashMap.KeySetView<Long, User> keySetView = data.keySet();
		Iterator<Long> iterator = keySetView.iterator();

		long time = System.currentTimeMillis();
		String output = time + ": ";

		while (iterator.hasNext()) {
			Long key = iterator.next();
			User value = data.getOrDefault(key,
					new User("username", "password", Gender.MAN));
			output += key + "=>" + value + "; ";
		}

		return output;

	}

	@GetMapping(value = "/json", produces = { MediaType.APPLICATION_JSON_VALUE })
	public User json() {
		return new User("username", "password", Gender.MAN);
	}

}
