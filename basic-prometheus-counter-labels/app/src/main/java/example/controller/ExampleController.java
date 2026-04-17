package example.controller;

import java.util.Arrays;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.util.List;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.web.bind.annotation.RestController;

import com.sun.el.stream.Optional;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import org.springframework.stereotype.Service;

@RestController
public class ExampleController {

	private final MeterRegistry registry;

	public ExampleController(MeterRegistry registry) {
		this.registry = registry;
	}

	@PostMapping("/analyze")
	public String analyze(@RequestParam(name = "data", required = false) String data) {

		List<String> lines = Arrays
				.asList((data == null) ? new String[] { "ooooooooooooooo", "ooooooxooooooooo", "ooooxxxxoooxooxoo", "" }
						: data.split("\\n"));
		for (String line : lines) {

			String status = classify(line);

			Counter.builder("rows_processed").description("Rows processed by status").tags("status", status)
					.register(registry).increment();
		}

		return "processed " + lines.size() + " rows";
	}

	private String classify(String line) {
		if (line == null || line.trim().isEmpty()) {
			return "failing";
		}

		long errors = line.chars().filter(c -> c == 'x').count();
		return (errors == 0) ? "perfect" : (errors < 3) ? "problematic" : "failing";
	}
}
