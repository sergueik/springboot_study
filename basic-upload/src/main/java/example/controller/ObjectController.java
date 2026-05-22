package example.controller;

import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Base64;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.dto.UploadRequest;

// uplods potentially heavy payloads, expects submitter to compose the JSON with unusual layout:
/*
 * { "foo": "alpha", "bar": "beta", "filename": "sample.txt", "contentType":
 * "text/plain", "contentBase64": "TGluZTEKTGluZTIK" }
 */
//  echo -n "Hello, World!" | base64 -
// SGVsbG8sIFdvcmxkIQ==
// echo "TGluZTEKTGluZTIK" | base64 -d -
// Line1
// Line2
//

@RestController
@RequestMapping("/api/upload")
public class ObjectController {

	private static final Logger logger = LoggerFactory.getLogger(ObjectController.class);

	// delegates DTO binding and validation during deserialization to Jackson - more strict
	@PostMapping("/binding")
	public ResponseEntity<String> uploadJson(@RequestBody UploadRequest request) {
		logger.info("automatic binding endpoint");
		logger.info("received: {}", request);
		return ResponseEntity.status(201).body("received: " + request.toString());
	}

	@ExceptionHandler({ IllegalArgumentException.class })
	public ResponseEntity<String> handleBadPayload(Exception e) {

		logger.error("Invalid payload", e);
		return ResponseEntity.unprocessableEntity().body("Could not deserialize UploadRequest");
	}
}
