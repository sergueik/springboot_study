package example.controller;

import java.util.HashMap;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;

import org.springframework.web.bind.annotation.CrossOrigin;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.File;
import java.nio.file.Paths;

import me.desair.tus.server.TusFileUploadService;
import me.desair.tus.server.exception.TusException;
import me.desair.tus.server.upload.UploadInfo;

import example.dto.ValidateRequest;
import example.service.DigestService;

@Controller
@RequestMapping(value = "/api/uploads")

@CrossOrigin(origins = "*")
public class ValidateController {

	private static final Logger logger = LoggerFactory.getLogger(ValidateController.class);
	@Autowired
	private TusFileUploadService tusFileUploadService;

	@Autowired
	private DigestService digestService;

	@PostMapping("/validate")
	public ResponseEntity<?> uploadJson(@RequestBody ValidateRequest request) throws Exception {
		UploadInfo info = null;
		String uploadURI = null;
		String hash = null;
		HttpStatus status = HttpStatus.BAD_REQUEST;
		Map<String, Object> data = new HashMap<>();
		data.put("status", "UNKNOWN");
		try {
			String uploadId = request.getUploadId();
			String uploadHash = request.getHash();
			data.put("uploadId", uploadId);
			data.put("uploadHash", uploadHash);
			logger.info("Loading request: {}", data);
			uploadURI = String.format("/api/upload/%s", uploadId);
			info = tusFileUploadService.getUploadInfo(uploadURI);
			if (info == null) {
				data.put("status", "NOT FOUND");
				status = HttpStatus.NOT_FOUND;
			} else {
				String inputFile = Paths.get(String.format("%s%starget%sdata%s%s", System.getProperty("user.dir"),
						File.separator, File.separator, File.separator, uploadId)).toString();
				logger.info("validate hash of the upload {} {}", inputFile, uploadHash);
				hash = digestService.digest(inputFile);
				data.put("filename", info.getFileName());
				data.put("hash", hash);

				if (hash.equalsIgnoreCase(uploadHash)) {
					data.put("status", "OK");
					status = HttpStatus.OK;
					logger.info("delete the upload {}", uploadURI);
					tusFileUploadService.deleteUpload(uploadURI);
					logger.info("cleanup");
					tusFileUploadService.cleanup();
				} else {
					data.put("status", "MISMATCH");
					status = HttpStatus.CONFLICT;
				}
			}
			logger.info("Returning status: {}", data);
			return ResponseEntity.status(status).body(data);
		} catch (TusException e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
		}
	}
}
