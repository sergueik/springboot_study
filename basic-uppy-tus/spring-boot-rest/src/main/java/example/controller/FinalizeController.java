package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;

import org.springframework.core.env.Environment;
import org.springframework.http.HttpStatus;
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
import example.service.FinalizeService;

@Controller
@RequestMapping(value = "/api/uploads")

@CrossOrigin(origins = "*")
public class FinalizeController {

	private static final Logger logger = LoggerFactory.getLogger(FinalizeController.class);
	@Autowired
	private TusFileUploadService tusFileUploadService;

	@Autowired
	private FinalizeService finalizeService;

	@PostMapping("/finalize")
	public ResponseEntity<?> uploadJson(@RequestBody FinalizeRequest request) throws Exception {
		UploadInfo info = null;
		String uploadURI = null;
		HttpStatus status = HttpStatus.BAD_REQUEST;
		Map<String, Object> data = new HashMap<>();
		data.put("status", "UNKNOWN");
		try {
			String uploadId = request.getUploadId();
			data.put("uploadId", uploadId);
			uploadURI = String.format("/api/upload/%s", uploadId);
			info = tusFileUploadService.getUploadInfo(uploadURI);
			if (info == null) {
				data.put("status", "NOT FOUND");
				status = HttpStatus.NOT_FOUND;
			} else if (info.isUploadInProgress()) {
				data.put("status", "IN PROGRESS");
				status = HttpStatus.ACCEPTED;
			} else {
				data.put("status", "OK");
				data.put("filename", info.getFileName());
				status = HttpStatus.OK;
				finalizeService.finalizeUpload(info);
			}
			logger.info("Returning status: {}", data);
			return ResponseEntity.status(status).body(data);
		} catch (TusException e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
		}
	}
}
