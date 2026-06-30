package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import org.springframework.core.env.Environment;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import me.desair.tus.server.TusFileUploadService;
import me.desair.tus.server.exception.TusException;
import me.desair.tus.server.upload.UploadInfo;

import example.dto.FinalizeRequest;
import example.service.DotnetConfigService;
import example.service.FinalizeService;

@Controller
@RequestMapping(value = "/api/uploads")

@CrossOrigin(origins = "*")
public class DotnetConfigController {

	private static final Logger logger = LoggerFactory.getLogger(DotnetConfigController.class);
	@Autowired
	private DotnetConfigService dotnetConfigService;

	@PostMapping("/config")
	@RequestMapping(value = { "/config" }, method = { RequestMethod.GET }, produces = MediaType.APPLICATION_JSON_VALUE)
	public ResponseEntity<?> config() throws Exception {
		dotnetConfigService.load();
		HttpStatus status = HttpStatus.BAD_REQUEST;
		Map<String, Object> data = new HashMap<>();
		data.put("TUS_CHUNK_SIZE", System.getProperty("tus.chunk.size"));
		logger.info("Returning config: {}", data);
		return ResponseEntity.status(HttpStatus.OK).body(data);
	}
}
