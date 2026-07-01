package example.controller;

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.service.DotnetConfigService;

@Controller
@RequestMapping(value = "/api/uploads")

@CrossOrigin(origins = "*")
public class DotnetConfigController {

	private static final Logger logger = LoggerFactory.getLogger(DotnetConfigController.class);
	@Autowired
	private DotnetConfigService dotnetConfigService;

	@PostMapping("/config")
	@RequestMapping(value = { "/config" }, method = { RequestMethod.GET }, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, Object>> config() throws Exception {
		dotnetConfigService.load();
		HttpStatus status = HttpStatus.BAD_REQUEST;
		Map<String, Object> data = new HashMap<>();
		// chop the "VITE_" prefix.
		// NOTE: if not set chunkSize, tus behaves like: send the whole file in one
		// request (effectively no chunking override)
		if (System.getProperties().containsKey("vite.tus.chunk.size"))
			data.put("TUS_CHUNK_SIZE", Long.parseLong(System.getProperty("vite.tus.chunk.size")));
		data.put("TUS_ENDPOINT", System.getProperty("vite.tus.endpoint"));
		if (System.getProperties().containsKey("vite.max.number.of.files"))
			data.put("MAX_NUMBER_OF_FILES", Integer.parseInt(System.getProperty("vite.max.number.of.files")));
		if (System.getProperties().containsKey("vite.max.file.size.bytes"))
			data.put("MAX_FILE_SIZE_BYTES", Long.parseLong(System.getProperty("vite.max.file.size.bytes")));
		data.put("TUS_RETRY_DELAYS", System.getProperty("vite.tus.retry.delays").split(","));

		logger.info("Returning config: {}", data);
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}
}
