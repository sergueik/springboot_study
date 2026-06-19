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

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.DigestInputStream;
import java.security.MessageDigest;

import me.desair.tus.server.TusFileUploadService;
import me.desair.tus.server.exception.TusException;
import me.desair.tus.server.upload.UploadInfo;

import example.dto.ValidateRequest;
import example.utils.TusStorageResolver;

@Controller
@RequestMapping(value = "/api/uploads")

@CrossOrigin(origins = "*")
public class ValidateController {

	private static final Logger logger = LoggerFactory.getLogger(ValidateController.class);
	@Autowired
	private TusFileUploadService tusFileUploadService;
	private static boolean debug = true;
	String inputFile = null;
	String hash = null;
	@Autowired
	private TusStorageResolver tusStorageResolver;

	@PostMapping("/validate")
	public ResponseEntity<?> uploadJson(@RequestBody ValidateRequest request) throws Exception {
		UploadInfo info = null;
		HttpStatus status = HttpStatus.BAD_REQUEST;
		Map<String, Object> data = new HashMap<>();
		data.put("status", "UNKNOWN");
		try {
			String uploadId = request.getUploadId();
			String uploadHash = request.getHash();
			data.put("uploadId", uploadId);
			data.put("uploadHash", uploadHash);
			logger.info("Loading request: {}", data);
			info = tusFileUploadService.getUploadInfo(String.format("/api/upload/%s", uploadId));
			if (info == null) {
				data.put("status", "NOT FOUND");
				status = HttpStatus.NOT_FOUND;
			} else {
				inputFile = tusStorageResolver.resolve(info).toAbsolutePath().toString();
				digest();
				data.put("filename", info.getFileName());
				data.put("hash", this.hash);
				if (this.hash.equalsIgnoreCase(uploadHash)) {
					data.put("status", "OK");
					status = HttpStatus.OK;
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

	public void digest() throws Exception {

		long start = System.currentTimeMillis();
		MessageDigest md = MessageDigest.getInstance("SHA-256");
		try (InputStream is = Files.newInputStream(Paths.get(inputFile));
				DigestInputStream dis = new DigestInputStream(is, md)) {

			// Read the stream fully to EOF to update the digest
			byte[] buffer = new byte[8192];
			while (dis.read(buffer) != -1)
				;
		}
		// Final hash
		byte[] digest = md.digest();
		hash = byteArrayToHex(digest);
		logger.info("Digest input: {} hash: {}", inputFile, hash);
		long end = System.currentTimeMillis();
		logger.info("Processed {} bytes in {} ms", new File(Paths.get(inputFile).toString()).length(), (end - start));
	}

	public static String byteArrayToHex(byte[] bytes) {
		StringBuilder sb = new StringBuilder(bytes.length * 2);
		for (byte b : bytes) {
			sb.append(String.format("%02X", b));
		}
		return sb.toString();
	}
}
