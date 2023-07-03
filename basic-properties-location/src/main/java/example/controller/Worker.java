package example.controller;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/")
public class Worker {

	@Autowired
	Properties properties;

	private final String fileName = "key.txt";

	@GetMapping("/check")
	public String propertyCheck() {
		StringBuilder response = new StringBuilder();

		try {
			String filePath = new File(
					Paths.get(System.getProperty("user.dir"))
							.resolve(String.join(System.getProperty("file.separator"),
									Arrays.asList(properties.getProperty("location", "."),
											fileName)))
							.toAbsolutePath().toString()).getCanonicalPath();
			response.append("Key file path: " + filePath + "\n");
			response
					.append("Data: " + new String(Files.readAllBytes(Paths.get(filePath)),
							StandardCharsets.UTF_8));
		} catch (IOException e) {
			response.append("Key file path: " + "error" + "\n");
		}

		return response.toString();
	}

}
