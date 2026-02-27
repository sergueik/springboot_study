package example.controller;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

// see also: https://jkoder.com/gson-encoding-the-string-like-u003d/

@RestController
@RequestMapping("/htmlescape")
public class HtmlEscapeController {

	private static Map<String, String> data = new HashMap<>();
	static {
		data.put("data", "0=RUNNING,1=RUNNING,2=RUNNING");
	}

	@ResponseBody
	@GetMapping(value = "/basic", produces = { MediaType.APPLICATION_JSON_VALUE })
	public String basic(@RequestParam Optional<Boolean> fix) {
		Gson gson = (fix.isPresent() && fix.get())
				? new GsonBuilder().disableHtmlEscaping().create() : new Gson();
		String json = gson.toJson(data);

		return json;
	}

	@ResponseBody
	@GetMapping(value = "/legacy", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<Map<String, String>> legacy(
			@RequestParam Optional<Boolean> fix) {
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}

	// NOTE: default Content-Type is "text/plain; charset=us-ascii"
	// for JSON it also appears platform dependent
	@ResponseBody
	@GetMapping(value = "/with_charset", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<String> basic_with_charset(
			@RequestParam Optional<Boolean> fix) {
		HttpHeaders headers = new HttpHeaders();
		Gson gson = (fix.isPresent() && fix.get())
				? new GsonBuilder().disableHtmlEscaping().create() : new Gson();
		String json = gson.toJson(data);
		headers.add("Content-Type", "application/json;charset=UTF-8");
		return new ResponseEntity<String>(json, headers, HttpStatus.OK);
	}

}
