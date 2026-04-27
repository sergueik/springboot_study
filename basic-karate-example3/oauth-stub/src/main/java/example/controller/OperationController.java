package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;

import com.google.gson.FieldNamingStrategy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import java.util.Map;
import java.util.UUID;
import java.util.concurrent.ConcurrentHashMap;

import example.dto.OperationSerializer;
// import example.dto.Operation;

@RestController
@RequestMapping("/operation")
@Tag(name = "Operation API", description = "CRUD operations secured by JWT")
@SecurityRequirement(name = "bearerAuth")
public class OperationController {
	@Value("${convertion:false}")
	private boolean conversion;

	private static final Logger log = LoggerFactory.getLogger(OperationController.class);
	private Gson gson = conversion ? new Gson()
			: new GsonBuilder().registerTypeAdapter(Operation.class, new OperationSerializer()).create();

	private final Map<String, String> store = new ConcurrentHashMap<>();

	@Operation(summary = "Create operation")
	@PostMapping(consumes = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, String>> create(@RequestBody Map<String, String> body) {

		String what = body.getOrDefault("what", "");
		String id = UUID.randomUUID().toString();
		log.info("created {}", gson.toJson(Map.of("id", id, "what", what)));

		store.put(id, what);
		return ResponseEntity.status(201).body(Map.of("id", id));
	}

	@Operation(summary = "Update operation by id")
	@PutMapping(value = "/{id}", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Void> update(@PathVariable String id, @RequestBody Map<String, String> body) {

		if (!store.containsKey(id)) {
			log.info("not found id: {}", id);
			return ResponseEntity.notFound().build();
		}

		store.put(id, body.getOrDefault("what", ""));
		// NOTE: simplified logging
		log.info("updated {}", gson.toJson(Map.of("id", id, "what", store.get(id))));
		// log.info("updated: {}", gson.toJson(store.get(id)));
		return ResponseEntity.ok().build();
	}

	@Operation(summary = "Delete operation by id")
	@DeleteMapping("/{id}")
	public ResponseEntity<Void> delete(@PathVariable String id) {

		store.remove(id);
		log.info("removed: {}", id);
		return ResponseEntity.noContent().build();
	}

	@Operation(summary = "Get operation by id")
	@GetMapping("/{id}")
	public ResponseEntity<Map<String, String>> get(@PathVariable String id) {

		String value = store.get(id);
		log.info("{}: {}", ((value == null) ? "not found" : "found"), id);
		return (value == null) ? ResponseEntity.notFound().build() : ResponseEntity.ok(Map.of("id", id, "what", value));
	}
}