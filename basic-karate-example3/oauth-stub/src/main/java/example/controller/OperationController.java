package example.controller;

import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
// import org.springframework.web.bind.annotation.*;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

@RestController
public class OperationController {

	private final Map<String, String> store = new ConcurrentHashMap<>();

	@PostMapping(value = "/operation", consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, String>> create(@RequestBody Map<String, String> body) {

		String what = body.getOrDefault("what", "");
		String id = UUID.randomUUID().toString();

		store.put(id, what);

		return ResponseEntity.status(201).body(Map.of("id", id));
	}

	@PutMapping(value = "/operation/{id}", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Void> update(@PathVariable String id, @RequestBody Map<String, String> body) {

		if (!store.containsKey(id)) {
			return ResponseEntity.notFound().build();
		}

		store.put(id, body.getOrDefault("what", ""));
		return ResponseEntity.ok().build();
	}

	@DeleteMapping("/operation/{id}")
	public ResponseEntity<Void> delete(@PathVariable String id) {

		store.remove(id);
		return ResponseEntity.noContent().build();
	}

	@GetMapping("/operation/{id}")
	public ResponseEntity<Map<String, String>> get(@PathVariable String id) {

		String value = store.get(id);

		if (value == null) {
			return ResponseEntity.notFound().build();
		}

		return ResponseEntity.ok(Map.of("id", id, "what", value));
	}
}