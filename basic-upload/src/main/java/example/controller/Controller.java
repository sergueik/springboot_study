package example.controller;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

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

	private static final StringBuilder data = new StringBuilder();

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
			try {
				System.err.println("Processing " + file.getOriginalFilename());
				data.setLength(0);
				InputStream in = file.getInputStream();
				String currDirPath = new File(".").getAbsolutePath();
				FileOutputStream f = new FileOutputStream(
						currDirPath.substring(0, currDirPath.length() - 1)
								+ file.getOriginalFilename());
				int ch = 0;
				while ((ch = in.read()) != -1) {
					f.write(ch);
					data.append(new Character((char) ch).toString());
				}
				f.flush();
				f.close();
				System.err.print(data.toString());
			} catch (IOException e) {
				System.err.print("Exception (caught):" + e.toString());
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}
			return ResponseEntity.status(HttpStatus.OK).body(data.toString());
		}
	}
}
