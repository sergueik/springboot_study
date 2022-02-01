package example.controller;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import example.runner.CustomApplicationRunner;
import example.state.Data;

@RestController
@RequestMapping("/")
public class Controller {

	private Data data = null;

	@GetMapping(value = "/data", produces = MediaType.APPLICATION_JSON_VALUE)
	public String json() {
		data = Data.getInstance();
		final CustomApplicationRunner runner = data.getApplicationRunner();
		return (runner != null) ? runner.toString() : "";
	}

	@PostMapping(value = "/cancel", consumes = MediaType.APPLICATION_JSON_VALUE, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<String> cancel() {
		data = Data.getInstance();
		final CustomApplicationRunner runner = data.getApplicationRunner();
		if (runner != null) {
			runner.cancel();
			return ResponseEntity.ok().build();
		} else {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}

	}
}
