package example.controller;
/**
 * Copyright 2026 Serguei Kouzmine
 */


import java.util.List;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.web.bind.annotation.RestController;

import io.micrometer.core.instrument.Counter;
import io.micrometer.core.instrument.MeterRegistry;
import org.springframework.stereotype.Service;


@RestController
public class ExampleController {

    private final MeterRegistry registry;

    public ExampleController(MeterRegistry registry) {
        this.registry = registry;
    }

    @GetMapping("/analyze")
    public String analyze() {

        List<String> lines = List.of(
                "ooooooooooooooo",
                "ooooooxooooooooo",
                "ooooxxxxoooxooxoo",
                ""
        );

        for (String line : lines) {

            String status = classify(line);

            Counter.builder("file_rows_processed_total")
                    .description("Rows processed by status")
                    .tags("status", status)
                    .register(registry)
                    .increment();
        }

        return "processed " + lines.size() + " rows";
    }

    private String classify(String line) {
        if (line == null || line.trim().isEmpty()) {
            return "failing";
        }

        long errors = line.chars().filter(c -> c == 'x').count();

        if (errors == 0) {
            return "perfect";
        } else if (errors < 3) {
            return "problematic";
        } else {
            return "failing";
        }
    }
}
