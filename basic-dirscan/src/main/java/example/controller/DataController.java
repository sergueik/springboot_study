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
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

// based on: https://github.com/kolorobot/spring-boot-thymeleaf
@Controller
@RequestMapping("/")
public class DataController {
	private StringBuffer sb = new StringBuffer();
	private Log log = LogFactory.getLog(this.getClass());

	private String directoryName = System.getProperty("os.name").toLowerCase().contains("windows")
			? System.getenv("TEMP")
			: "/tmp";

	public String getDirectoryName() {
		return directoryName;
	}

	public void setDirectoryName(String data) {
		directoryName = data;
	}

	private void getFiles(String filename, List<String> files) throws RuntimeException {
		File file = null;
		BufferedReader reader = null;
		StringBuffer contents = new StringBuffer();
		String text = null;
		try {
			file = new File(filename);
			reader = new BufferedReader(new FileReader(file));
			while ((text = reader.readLine()) != null) {
				files.add(text.trim());
				contents.append(text).append(System.getProperty("line.separator"));
			}
			reader.close();
			// System.out.println(contents.toString());
		} catch (IOException e) {
			log.info("Exception: " + e.getMessage());
			throw new RuntimeException("Exception: " + e.getMessage());
		} finally {
			try {
				reader.close();
			} catch (IOException e) {
			}
		}
	}

	private void readFiles(final String directoryName, final List<String> files, final String key,
			final Map<String, String> data) throws RuntimeException {
		File dir = new File(directoryName);
		String[] children = dir.list();
		for (String filename : files) {
			data.put(filename, null);
		}
		if (children == null) {
			log.info(String.format("%s does not exist or is not al directory", directoryName));
			throw new RuntimeException("%s does not exist or is not al directory");
			// return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		for (int i = 0; i < children.length; i++) {
			String filename = children[i];
			if (files.contains(filename)) {
				String datafilename = String.format("%s/%s/data.txt", directoryName, filename);
				StringBuffer contents = new StringBuffer();
				BufferedReader reader = null;
				try {
					log.info(String.format("reading datafile %s", datafilename));
					reader = new BufferedReader(new FileReader(datafilename));
					String text = null;

					String textValue = null;
					String textKey = null;

					while ((text = reader.readLine()) != null) {
						contents.append(text).append(System.getProperty("line.separator"));
						Pattern p = Pattern.compile("^(\\S*):(.*)$");
						Matcher m = p.matcher(text);
						// reserved for future use
						log.info("scanning for " + key);
						if (m.find()) {
							textKey = m.group(1);
							textValue = m.group(2);
							if (textKey.equalsIgnoreCase(key)) {
								data.put(filename, textValue);
							}
						}
					}
					reader.close();
					log.info(String.format("datafile %s contents: %s", datafilename, contents.toString()));
				} catch (IOException e) {
					data.put(filename, null);
				}
			}
		}
		return;
	}

	@ResponseBody
	@GetMapping("/data/{name}/{key:.+}")
	public ResponseEntity<Map<String, String>> showData(@PathVariable("name") String name,
			@PathVariable("key") String key) {

		List<String> files = new ArrayList<>();
		Map<String, String> data = new HashMap<>();

		try {
			log.info(String.format("Read hosts list from %s", name));
			getFiles(String.format("%s/%s", directoryName, name), files);
			log.info(String.format("Read hosts list: %s", files));
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		try {
			log.info(String.format("Read key %s from hosts", key));
			readFiles(directoryName, files, key, data);
			return ResponseEntity.status(HttpStatus.OK).contentType(MediaType.APPLICATION_JSON_UTF8).body(data);
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

	@ResponseBody
	@GetMapping("/data")
	public ResponseEntity<Map<String, String>> showData(@RequestParam Optional<String> name,
			@RequestParam Optional<String> key) {
		List<String> files = new ArrayList<>();
		Map<String, String> data = new HashMap<>();

		String mapName = name.isPresent() ? String.format("%s/%s", directoryName, name.get()) : null;
		log.info(String.format("Read hosts list from %s", mapName));
		if (mapName != null) {
			try {
				getFiles(mapName, files);
				log.info(String.format("Read hosts list: %s", files));
			} catch (RuntimeException e) {
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}
		}

		String dataKey = key.isPresent() ? key.get() : "default";
		try {
			readFiles(directoryName, files, dataKey, data);
			return ResponseEntity.status(HttpStatus.OK).contentType(MediaType.APPLICATION_JSON_UTF8).body(data);

		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

}
