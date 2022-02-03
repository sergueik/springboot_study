package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import example.runner.CustomApplicationRunner;
import example.state.Data;

@RestController
@RequestMapping("/")
public class CustomController {

	// TODO: share instance with the controller
	private static final Logger logger = LoggerFactory
			.getLogger(CustomController.class);

	private Data data = null;

	@GetMapping(value = "/data", produces = MediaType.TEXT_PLAIN_VALUE)
	public String data() {

		data = Data.getInstance();
		final CustomApplicationRunner runner = data.getApplicationRunner();
		final String payload = (runner != null) ? runner.toString() : "";
		logger.info("returning: " + payload);
		return payload;

	}

	@SuppressWarnings("unused")
	@GetMapping(value = "/json", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<CustomApplicationRunner> json() {

		data = Data.getInstance();
		CustomApplicationRunner runner = data.getApplicationRunner();

		logger.info("returning: " + runner.toString());
		// NOTE: java.lang.StackOverflowError when serializng the object instance by
		// hand
		/*
		try {
			final Gson gson = new Gson();
			final String payload = gson.toJson(runner);
		} catch (java.lang.StackOverflowError e) {
			logger.info("caught (ingored) " + e.toString());
		}
		*/
		if (runner != null) {
			logger.info("json() method returning: " + runner.toString());
			return ResponseEntity.ok(runner);
		} else {
			logger.info("returning: null");
			return null;
		}
	}

	@PostMapping(value = "/cancel", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<String> cancel() {
		data = Data.getInstance();
		final CustomApplicationRunner runner = data.getApplicationRunner();
		if (runner != null) {
			logger.info("Invoking cancel method");
			runner.cancel();
			return ResponseEntity.ok().build();
		} else {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

	@GetMapping(value = "/dummyData", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<DummyData> dummyData() {

		DummyData dummyData = new DummyData();
		dummyData.setName("my name");

		// if (dummyData != null) {
			logger.info("dummyData() method returning: " + dummyData.toString());
			return ResponseEntity.ok(dummyData);
			// NOTE: "else"block willbe identified as dead code
			// NOTE: with commented, fails with This method must return a result of
			// type ResponseEntity<CustomController.DummyData>
			/*
			} else {
			logger.info("returning: null");
			return null;
			*/
		// }

	}

	public static class DummyData {

		private String name;

		public String getName() {
			return name;
		}

		public void setName(String DummyData) {
			name = DummyData;
		}

		public DummyData(String name) {
			this.name = name;
		}

		public DummyData() {
		}
	}

}
