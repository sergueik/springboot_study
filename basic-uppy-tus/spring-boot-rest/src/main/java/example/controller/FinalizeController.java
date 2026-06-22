package example.controller;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
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
import example.utils.TusStorageResolver;

@Controller
@RequestMapping(value = "/api/uploads")

@CrossOrigin(origins = "*")
public class FinalizeController {

	private static final Logger logger = LoggerFactory.getLogger(FinalizeController.class);
	@Autowired
	private TusFileUploadService tusFileUploadService;
	Path inputFilePath = null;
	@Autowired
	private TusStorageResolver tusStorageResolver;

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
				inputFilePath = tusStorageResolver.resolve(info).toAbsolutePath();
				Path targetFilePath = Paths.get(String.format("%s%starget%sdata%s%s", System.getProperty("user.dir"),
						File.separator, File.separator, File.separator, uploadId));
				mkdirs(targetFilePath.toFile());
				logger.info("move {} to {}", inputFilePath, targetFilePath);
				Files.move(inputFilePath, targetFilePath, StandardCopyOption.REPLACE_EXISTING);
			}
			logger.info("Returning status: {}", data);
			return ResponseEntity.status(status).body(data);
		} catch (TusException e) {
			return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body(null);
		}
	}

	public static void mkdirs(File dir) {
		File parent = dir.getAbsoluteFile();
		List<File> mkdir = new ArrayList<File>();
		for (; !parent.exists() || !parent.isDirectory(); parent = parent.getParentFile()) {
			mkdir.add(parent);
		}
		for (int i = mkdir.size(); --i >= 0;) {
			File d = mkdir.get(i);
			d.mkdir();
			d.setReadable(true, false);
			d.setWritable(true, false);
		}
	}

}
