package example.controller;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import me.desair.tus.server.TusFileUploadService;
import me.desair.tus.server.exception.TusException;
import me.desair.tus.server.upload.UploadInfo;
import example.dto.FinalizeRequest;
import example.dto.FinalizeRequest;

@Controller
@RequestMapping(value = "/api/uploads")

@CrossOrigin(origins = "*")
public class FinalizeController {

	private static final Logger logger = LoggerFactory.getLogger(FinalizeController.class);
	@Autowired
	private TusFileUploadService tusFileUploadService;

	@PostMapping("/finalize")
	public ResponseEntity<?> uploadJson(@RequestBody FinalizeRequest request) throws Exception {
		UploadInfo info = null;
		HttpStatus status = HttpStatus.BAD_REQUEST;
		try {
			Map<String, Object> data = new HashMap<>();
			String id = request.getId();
			data.put("id", id);
			info = tusFileUploadService.getUploadInfo(String.format("/api/upload/%s", request.getId()));
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
			}
			return ResponseEntity.status(status).body(data);
		} catch (TusException e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
		}
	}
}
