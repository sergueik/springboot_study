package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
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

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.component.DataComponent;
import example.domain.Gender;
import example.domain.User;

@RestController
@RequestMapping("/")
public class ExampleController {

	@Autowired
	DataComponent data;

	private static Gson gson;
	private boolean debug = false;

	public ExampleController() {

	}

	public ExampleController(DataComponent data, boolean debug) {
		this.data = data;
		this.debug = debug;
	}

	@RequestMapping(method = RequestMethod.GET, value = "/all", produces = {
			MediaType.TEXT_PLAIN_VALUE })
	public ResponseEntity<String> hello() {
		ConcurrentHashMap.KeySetView<Long, Map<Long, User>> keySetView = data
				.keySet();
		// NOTE: curently there is just one key in the data
		Iterator<Long> iterator = keySetView.iterator();

		long time = System.currentTimeMillis();
		Map<Long, User> cachedUsers = new HashMap<>();
		while (iterator.hasNext()) {
			Long key = iterator.next();
			cachedUsers = data.getOrDefault(key, new HashMap<>());

		}
		String output = time + ": ";
		for (Long key : cachedUsers.keySet()) {
			User value = cachedUsers.get(key);
			output += key + "=>" + value + "; ";
		}
		HttpHeaders headers = new HttpHeaders();
		headers.add("Content-Type", "text/plain;charset=us-ascii");
		return new ResponseEntity<String>(output, headers, HttpStatus.OK);

	}

	public Map<Long, User> getCachedUsers() {
		ConcurrentHashMap.KeySetView<Long, Map<Long, User>> keySetView = data
				.keySet();
		// NOTE: curently there is just one key in the data
		Iterator<Long> iterator = keySetView.iterator();

		long time = System.currentTimeMillis();
		Map<Long, User> cachedUsers = new HashMap<>();
		while (iterator.hasNext()) {
			Long key = iterator.next();
			cachedUsers = data.getOrDefault(key, new HashMap<>());

		}
		return cachedUsers;
	}

	public User getUser(Map<Long, User> cachedUsers, Long key) {
		return cachedUsers.get(key);
	}

	@ResponseBody
	@GetMapping(value = "/json/{key}", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<String> json(@PathVariable("key") Long key,
			@RequestParam Optional<Boolean> fix) {
		gson = (fix.isPresent() && fix.get())
				? new GsonBuilder().disableHtmlEscaping().create() : new Gson();
		String json = gson.toJson(getUser(getCachedUsers(), key));
		return new ResponseEntity<String>(json, HttpStatus.OK);
	}

	@ResponseBody
	@GetMapping(value = "/data/{key}", produces = { MediaType.TEXT_PLAIN_VALUE })
	public ResponseEntity<String> getProperty(@PathVariable("key") Long key,
			@RequestParam Optional<String> prop) {
		gson = new GsonBuilder().disableHtmlEscaping().create();
		String propString;
		if (prop.isPresent() && prop.get() != null) {
			propString = prop.get();
			User user = getUser(getCachedUsers(), key);
			String result = null;
			switch (propString) {
			case "name":
				result = user.getUserName();
				break;
			case "gender":
				result = user.getGender().toString();
				break;
			case "id":
				result = String.valueOf(user.getId());
				break;
			default:
				result = "unknown";
			}
			return new ResponseEntity<String>(result, HttpStatus.OK);
		} else {
			return null;
		}
	}

	/*
	@ResponseBody
	@GetMapping(value = "/json/{key}", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<String> json(@PathVariable("key") Long key,
			@RequestParam Optional<Boolean> fix) {
		gson = (fix.isPresent() && fix.get())
				? new GsonBuilder().disableHtmlEscaping().create() : new Gson();
		String json = gson.toJson(getUser(getCachedUsers(), key));
		return new ResponseEntity<String>(json, HttpStatus.OK);
	}
	
	*/
}
