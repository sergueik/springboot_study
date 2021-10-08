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
	public String hello() {
		return "hello";
	}

	@RequestMapping(value = "/upload", method = RequestMethod.POST)
	public ResponseEntity<String> upload(
			@RequestParam("file") MultipartFile file) {
		if (file.isEmpty()) {
			return ResponseEntity.status(HttpStatus.NOT_FOUND).body(null);
		}
		try {
			System.err.println("Processing " + file.getOriginalFilename());
			InputStream in = file.getInputStream();
			String currDirPath = new File(".").getAbsolutePath();
			FileOutputStream f = new FileOutputStream(
					currDirPath.substring(0, currDirPath.length() - 1)
							+ file.getOriginalFilename());
			int ch = 0;
			while ((ch = in.read()) != -1) {
				f.write(ch);
				System.err.print(String.format("%c", ch));
			}
			f.flush();
			f.close();
		} catch (IOException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		return ResponseEntity.status(HttpStatus.OK).body(null);
	}
}
