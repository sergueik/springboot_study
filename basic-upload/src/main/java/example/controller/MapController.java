package example.controller;

import java.util.Base64;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.dto.UploadRequest;

@RestController
@RequestMapping("/api/upload")
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
public class MapController {

	private static final Logger logger = LoggerFactory.getLogger(MapController.class);

	@PostMapping("/manual")
	public ResponseEntity<String> uploadJson(@RequestBody Map<String, String> payload) {
		logger.info("manual mapping endpoint");
		try {
			UploadRequest request = new UploadRequest();
			request.setBar(payload.get("bar"));
			request.setFoo(payload.get("foo"));
			request.setFilename(payload.get("filename"));
			request.setContentBase64(payload.get("contentBase64"));
			request.setContentType(payload.get("contentType"));
			logger.info("received: " + request.toString());
			return ResponseEntity.status(201).body("received: " + request.toString());
		} catch (IllegalArgumentException e) {
			return ResponseEntity.unprocessableEntity().body("...");
		}
	}
}
