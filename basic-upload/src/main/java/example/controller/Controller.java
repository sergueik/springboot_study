package example.controller;

import java.io.IOException;

import java.nio.charset.Charset;
import java.nio.file.Files;
import java.nio.file.Paths;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequestMapping("/basic")
public class Controller {

	@GetMapping(produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> hello() {
		return ResponseEntity.status(HttpStatus.OK).body(null);
	}

	@RequestMapping(value = "/upload", method = RequestMethod.POST)
	public ResponseEntity<String> upload(
			@RequestParam("operation") String operation,
			@RequestParam("param") String param,
			@RequestParam("file") MultipartFile file) {
		if (param.isEmpty())
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		if (!operation.equals("send")) {
			return ResponseEntity.status(HttpStatus.METHOD_NOT_ALLOWED).body(null);
		} else {
			if (file.isEmpty()) {
				return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
			}
			String value = null;
			try {
				System.err.println("Processing file:" + file.getOriginalFilename());
				String datafilePath = Paths.get(".").resolve(file.getOriginalFilename())
						.toAbsolutePath().toString();

				value = readFile(datafilePath, Charset.forName("UTF-8"));
				System.err.print(value);
			} catch (IOException e) {
				System.err.print("Exception (caught):" + e.toString());
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}
			return ResponseEntity.status(HttpStatus.OK).body(value);
		}
	}

	public static String readFile(String path, Charset encoding)
			throws IOException {
		byte[] encoded = Files.readAllBytes(Paths.get(path));
		return new String(encoded, encoding);
	}

}
