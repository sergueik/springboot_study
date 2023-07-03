package example.controller;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
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
	private Path path;
	private Path location;
	private StringBuilder response;
	private String filePath = null;

	@GetMapping("/check")
	public String propertyCheck() {
		response = new StringBuilder();
		location = Paths
				.get(new File(String.join(System.getProperty("file.separator"),
						Arrays.asList(System.getProperty("user.dir"),
								properties.getProperty("location"), fileName)))
										.getAbsolutePath());
		// does not remove ".." from the path
		response.append("Key file path: " + location.toString() + "\n");

		path = Paths.get(System.getProperty("user.dir"));
		location = path.resolve(String.join(System.getProperty("file.separator"),
				Arrays.asList(properties.getProperty("location"), fileName)));

		// does not remove ".." from the path
		response.append("Key file path: " + location.toString() + "\n");

		try {
			// see also:
			// https://www.digitalocean.com/community/tutorials/java-file-path-absolute-canonical
			path = Paths.get(System.getProperty("user.dir"));
			filePath = new File(path
					.resolve(String.join(System.getProperty("file.separator"),
							Arrays.asList(properties.getProperty("location"), fileName)))
					.toString()).getCanonicalPath();
			// does not remove ".." from the path
			response.append("Key file path: " + filePath + "\n");
			response.append("Data: "
					+ new String(Files.readAllBytes(location), StandardCharsets.UTF_8));
		} catch (IOException e) {

			// does not remove ".." from the path
			response.append("Key file path: " + "error" + "\n");

		}

		return response.toString();
	}

}
