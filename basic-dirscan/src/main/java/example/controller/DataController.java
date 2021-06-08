package example.controller;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

// based on: https://github.com/kolorobot/spring-boot-thymeleaf
@Controller
@RequestMapping("/")
public class DataController {

	private String directoryName = System.getProperty("os.name").toLowerCase()
			.contains("windows") ? System.getenv("TEMP") : "/tmp";

	public String getDirectoryName() {
		return directoryName;
	}

	public void setDirectoryName(String data) {
		directoryName = data;
	}

	private StringBuffer sb = new StringBuffer();

	private Log log = LogFactory.getLog(this.getClass());

	private void getFiles(String filename, List<String> files) {
		File file = null;
		BufferedReader reader = null;
		try {
			file = new File(filename);
			StringBuffer contents = new StringBuffer();

			reader = new BufferedReader(new FileReader(file));
			String text = null;

			// repeat until all lines is read
			while ((text = reader.readLine()) != null) {
				files.add(text.trim());
				contents.append(text).append(System.getProperty("line.separator"));
			}
			reader.close();
			System.out.println(contents.toString());
		} catch (IOException e) {
			e.printStackTrace();
		} finally {
			try {
				reader.close();
			} catch (IOException e) {
			}
		}
	}

	@ResponseBody
	@GetMapping("/data")
	public ResponseEntity<Map<String, String>> showData(
			@RequestParam Optional<String> name, @RequestParam Optional<String> key) {
		String mapName = name.isPresent()
				? String.format("%s/%s", directoryName, name.get()) : null;
		log.info(String.format("Read hosts list from %s", mapName));
		List<String> files = new ArrayList<>();
		if (mapName != null) {
			getFiles(mapName, files);
			log.info(String.format("Read hosts list: %s", files));
		}
		Map<String, String> data = new HashMap<>();
		for (String filename : files) {
			data.put(filename, null);
		}
		File dir = new File(directoryName);
		String[] children = dir.list();
		if (children == null) {
			log.info(String.format("%s does not exist or is not al directory",
					directoryName));
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		} else {
			for (int i = 0; i < children.length; i++) {
				// temporarily read all
				String filename = children[i];
				if (files.contains(filename)) {
					String datafilename = String.format("%s/%s/data.txt", directoryName,
							filename);
					StringBuffer contents = new StringBuffer();
					BufferedReader reader = null;
					try {
						log.info(String.format("reading datafile %s", datafilename));
						reader = new BufferedReader(new FileReader(datafilename));
						String text = null;

						// repeat until all lines is read
						String textValue = null;
						String textKey = null;

						while ((text = reader.readLine()) != null) {
							contents.append(text)
									.append(System.getProperty("line.separator"));
							Pattern p = Pattern.compile("^(\\S*):(.*)$");
							Matcher m = p.matcher(text);
							String dataKey = key.isPresent() ? key.get() : "default";
							// reserved for future use
							log.info("scanning for " + dataKey);
							if (m.find()) {
								textKey = m.group(1);
								textValue = m.group(2);
								if (textKey.equalsIgnoreCase(dataKey)) {
									data.put(filename, textValue);
								}
							}
						}
						reader.close();
						log.info(String.format("datafile %s contents: %s", datafilename,
								contents.toString()));
					} catch (IOException e) {
						data.put(filename, null);
					}
				}
			}
			return ResponseEntity.status(HttpStatus.OK)
					.contentType(MediaType.APPLICATION_JSON_UTF8).body(data);
		}
	}

}
