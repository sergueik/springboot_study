package example.controller;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.Arrays;
import java.util.Properties;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
@RequestMapping("/")
public class Worker {

	private static final Logger logger = LoggerFactory.getLogger(Worker.class);

	@Autowired
	Properties properties;

	public Worker(Properties data) {
		properties = data;
	}

	private final String fileName = "key.txt";

	@GetMapping("/check")
	public String propertyCheck() {
		StringBuilder response = new StringBuilder();
		String filePath = null;
		String pathCandidate = null;
		try {
			pathCandidate = String.join(System.getProperty("file.separator"),
					Arrays.asList(properties.getProperty("location", "."), fileName));
			filePath = new File(
					(Paths.get((pathCandidate.startsWith("/")) ? pathCandidate
							: System.getProperty("user.dir")).resolve(pathCandidate))
									.toAbsolutePath().toString()).getCanonicalPath();
			logger.info("Read file from: " + filePath);
			Path path = Paths.get(filePath);
			if (Files.isReadable(path)) {
				String data = new String(Files.readAllBytes(Paths.get(filePath)),
						StandardCharsets.UTF_8);
				logger.info(String.format("Read  %d bytes", data.length()));
				response.append("Data: " + data);
			} else {
				logger.error("Invalid path to the file: "
						+ (filePath == null ? pathCandidate : filePath));
				response.append("Data: read error: "
						+ (filePath == null ? pathCandidate : filePath));
			}
		} catch (IOException e) {
			logger.error("Key file path error: "
					+ (filePath == null ? pathCandidate : filePath) + " "
					+ e.getMessage());
			response.append("Data: Key file path error: "
					+ (filePath == null ? pathCandidate : filePath) + " "
					+ e.getMessage());
		}

		return response.toString();
	}

}
