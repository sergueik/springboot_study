package example.controller;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import example.service.ServerService;

@Controller
@RequestMapping("/")
public class DataController {
	private boolean debug = false;

	public void setDebug(boolean data) {
		debug = data;
	}

	private Log log = LogFactory.getLog(this.getClass());

	private String baseDirectory = System.getProperty("os.name").toLowerCase()
			.contains("windows") ? System.getenv("TEMP") : "/tmp";

	/*
	private final ServerService service;
	
	@Autowired
	public DataController(ServerService data) {
	service = data;
	}
	*/
	@ResponseBody
	@GetMapping(value = { "/server/{name}" })
	public ResponseEntity<List<String>> showServers(@PathVariable String name) {
		List<String> data = new ArrayList<>();
		try {
			ServerService serverService = new ServerService(name);
			data = serverService.getServers();
			System.err.println("Returning data: " + data);
			return ResponseEntity.status(HttpStatus.OK)
					.contentType(MediaType.APPLICATION_JSON_UTF8).body(data);
		} catch (RuntimeException e) {
			System.err.println("Returning error: " + e.getMessage());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST)
					.body(Arrays.asList(e.getMessage()));
		}
	}
}
