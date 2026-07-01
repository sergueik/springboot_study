package example.controller;

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import com.fasterxml.jackson.databind.ObjectMapper;

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
	@Autowired
	private ObjectMapper objectMapper;

	@PostMapping("/config")
	@RequestMapping(value = { "/config" }, method = { RequestMethod.GET }, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, Object>> config() throws Exception {
		Map<String, Object> config = dotnetConfigService.buildConfig();
		logger.info("Returning config: {}", objectMapper.writeValueAsString(config));
		return ResponseEntity.status(HttpStatus.OK).body(config);
	}

	@GetMapping(value = "/config.js", produces = "application/javascript")
	@ResponseBody
	public String configJs() throws Exception {

		Map<String, Object> config = dotnetConfigService.buildConfig();
		String javascript = "window.APP_CONFIG = " + objectMapper.writeValueAsString(config) + ";";
		logger.info("Returning javascript: {}", javascript);

		return javascript;
	}
}
