package example.controller;

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import com.fasterxml.jackson.databind.ObjectMapper;

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

	// NOTE: for error reporting do not return empty body, but a meaningful
	// {};
	@GetMapping(value = "/config", produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<Map<String, Object>> config() throws Exception {
		Map<String, Object> config = dotnetConfigService.buildConfig();
		logger.info("Returning config JSON: {}", objectMapper.writeValueAsString(config));
		return ResponseEntity.status(HttpStatus.OK).body(config);
	}

	// NOTE: there is no "MediaType..APPLICATION_JAVASCRIPT_VALUE"
	// NOTE: for error reporting do not return empty body, but a meaningful
	// window.APP_CONFIG = {};
	// window.APP_CONFIG_ERROR = "Failed to build runtime configuration";
	// then the browser still executes valid JavaScript, and the page can inspect:
	@GetMapping(value = "/config.js", produces = "application/javascript")
	@ResponseBody
	public ResponseEntity<String> configJs() throws Exception {
		try {
			Map<String, Object> config = dotnetConfigService.buildConfig();
			String javascript = "window.APP_CONFIG = " + objectMapper.writeValueAsString(config) + ";";
			logger.info("Returning javascript: {}", javascript);

			return ResponseEntity.status(HttpStatus.OK).body(javascript);
		} catch (Exception e) {
			logger.info("Exception: {}", e);
			var errorPayload = "window.APP_CONFIG = {}; window.APP_CONFIG_ERROR = 'Failed to build runtime configuration'";
			return ResponseEntity.status(HttpStatus.SERVICE_UNAVAILABLE).body(errorPayload);
		}
	}
}
