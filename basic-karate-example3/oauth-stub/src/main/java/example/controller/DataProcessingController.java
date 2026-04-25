package example.controller;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
public class DataProcessingController {
	@PostMapping(value = "/processdata", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, Object>> processJson(@RequestBody Map<String, Object> payload,
			Authentication auth) {

		String customer = (String) payload.getOrDefault("customer", "unknown");
		String what = (String) payload.getOrDefault("what", "");
		String username = auth != null ? auth.getName() : "anonymous";

		Map<String, Object> response = new LinkedHashMap<>();

		response.put("customerId", customer);
		response.put("messageType", "processed");
		response.put("user", username);
		response.put("payload", Map.of("originalWhat", what, "normalizedWhat", what.toUpperCase(), "length",
				what.length(), "isEmpty", what.isEmpty()));

		return ResponseEntity.ok(response);
	}
}