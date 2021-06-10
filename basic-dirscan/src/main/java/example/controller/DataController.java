package example.controller;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
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
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseBody;

import example.model.DataRow;
import example.model.HostDataRow;

@Controller
@RequestMapping("/")
public class DataController {
	private Log log = LogFactory.getLog(this.getClass());

	private String baseDirectory = System.getProperty("os.name").toLowerCase()
			.contains("windows") ? System.getenv("TEMP") : "/tmp";

	@ResponseBody
	@GetMapping("/data/{name}/{key}")
	public ResponseEntity<Map<String, String>> showData(
			@PathVariable("name") String name, @PathVariable("key") String key) {

		List<String> hostDirs = new ArrayList<>();
		Map<String, String> data = new HashMap<>();

		try {
			log.info(String.format("Read hosts list from %s", name));
			getHostDirs(String.format("%s/%s", baseDirectory, name), hostDirs);
			log.info(String.format("Read hosts list: %s", hostDirs));
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		try {
			log.info(String.format("Read key %s from hosts", key));
			readHostDirs(baseDirectory, hostDirs, key, data);
			return ResponseEntity.status(HttpStatus.OK)
					.contentType(MediaType.APPLICATION_JSON_UTF8).body(data);
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

	@ResponseBody
	@GetMapping(value = { "/data_param_map/{name}/{key}",
			"/data_param_map/{name}/{key}/{cnt}" })
	public ResponseEntity<List<DataRow>> showTypedData(
			@PathVariable Map<String, String> pathVariableMap) {
		return pathVariableMap.containsKey("cnt")
				? showTypedData(pathVariableMap.get("name"), pathVariableMap.get("key"),
						Integer.parseInt(pathVariableMap.get("cnt")))
				: showTypedData(pathVariableMap.get("name"),
						pathVariableMap.get("key"));
	}

	@ResponseBody
	@RequestMapping(method = RequestMethod.GET, value = {
			"/typeddata/{name}/{key}", "/typeddata/{name}/{key}/{cnt}" })
	public ResponseEntity<List<DataRow>> showTypedData(
			@PathVariable("name") String name, @PathVariable("key") String key,
			@PathVariable(required = false) Integer cnt) {
		if (cnt != null) {
			List<String> hostDirs = new ArrayList<>();
			Map<String, String> data = new HashMap<>();

			try {
				log.info(String.format("Read max %d hosts list from %s", cnt, name));
				getHostDirs(String.format("%s/%s", baseDirectory, name), hostDirs, cnt);
				log.info(String.format("Read hosts list: %s", hostDirs));
			} catch (RuntimeException e) {
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}
			try {
				log.info(String.format("Read key %s from hosts", key));
				readHostDirs(baseDirectory, hostDirs, key, data);
				List<DataRow> dataRows = new ArrayList<>();
				formatData(data, key, dataRows);
				return ResponseEntity.status(HttpStatus.OK)
						.contentType(MediaType.APPLICATION_JSON_UTF8).body(dataRows);
			} catch (RuntimeException e) {
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}

		} else {
			return showTypedData(name, key);
		}
	}

	@ResponseBody
	@GetMapping("/typeddata")
	public ResponseEntity<List<DataRow>> showTypedData(@RequestParam String name,
			@RequestParam("key") String key) {

		List<String> hostDirs = new ArrayList<>();
		Map<String, String> data = new HashMap<>();

		try {
			log.info(String.format("Read hosts list from %s", name));
			getHostDirs(String.format("%s/%s", baseDirectory, name), hostDirs);
			log.info(String.format("Read hosts list: %s", hostDirs));
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		try {
			log.info(String.format("Read key %s from hosts", key));
			readHostDirs(baseDirectory, hostDirs, key, data);
			List<DataRow> dataRows = new ArrayList<>();
			formatData(data, key, dataRows);
			return ResponseEntity.status(HttpStatus.OK)
					.contentType(MediaType.APPLICATION_JSON_UTF8).body(dataRows);
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

	@ResponseBody
	@GetMapping("/data")
	public ResponseEntity<Map<String, String>> showData(
			@RequestParam Optional<String> name, @RequestParam Optional<String> key) {
		List<String> hostDirs = new ArrayList<>();
		Map<String, String> data = new HashMap<>();

		String mapName = name.isPresent()
				? String.format("%s/%s", baseDirectory, name.get()) : null;
		log.info(String.format("Read hosts dir list from %s", mapName));
		if (mapName != null) {
			try {
				getHostDirs(mapName, hostDirs);
				log.info(String.format("Read hosts list: %s", hostDirs));
			} catch (RuntimeException e) {
				return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
			}
		}

		String dataKey = key.isPresent() ? key.get() : "default";
		try {
			readHostDirs(baseDirectory, hostDirs, dataKey, data);
			return ResponseEntity.status(HttpStatus.OK)
					.contentType(MediaType.APPLICATION_JSON_UTF8).body(data);

		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

	@ResponseBody
	@GetMapping("/typeddata_v2")
	public ResponseEntity<Map<String, HostDataRow>> showTypedDataVersion2(
			@RequestParam String name, @RequestParam("key") String key) {

		List<String> hostDirs = new ArrayList<>();
		Map<String, String> data = new HashMap<>();

		try {
			log.info(String.format("Read hosts list from %s", name));
			getHostDirs(String.format("%s/%s", baseDirectory, name), hostDirs);
			log.info(String.format("Read hosts list: %s", hostDirs));
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		try {
			log.info(String.format("Read key %s from hosts", key));
			readHostDirs(baseDirectory, hostDirs, key, data);
			Map<String, HostDataRow> result = new HashMap<>();
			formatData(data, key, result);
			return ResponseEntity.status(HttpStatus.OK)
					.contentType(MediaType.APPLICATION_JSON_UTF8).body(result);
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

	public String getBaseDirectory() {
		return baseDirectory;
	}

	public void setBaseDirectory(String data) {
		baseDirectory = data;
	}

	private void getHostDirs(String filename, List<String> hostDirs)
			throws RuntimeException {
		getHostDirs(filename, hostDirs, 0);
	}

	private void getHostDirs(String filename, List<String> hostDirs, Integer cnt)
			throws RuntimeException {
		File file = null;
		BufferedReader reader = null;
		StringBuffer contents = new StringBuffer();
		String text = null;
		try {
			file = new File(filename);
			reader = new BufferedReader(new FileReader(file));
			while ((text = reader.readLine()) != null) {
				contents.append(text).append(System.getProperty("line.separator"));
				if (cnt != 0 && hostDirs.size() > cnt - 1) {
					continue;
				}
				hostDirs.add(text.trim());
			}
			reader.close();
			log.info("Read " + (cnt == 0 ? "" : "first " + cnt + " lines from ")
					+ ": " + contents.toString());
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

	private void readHostDirs(final String baseDirectory,
			final List<String> hostDirs, final String key,
			final Map<String, String> data) throws RuntimeException {
		File dir = new File(baseDirectory);
		String[] subdirs = dir.list();
		for (String dirName : hostDirs) {
			data.put(dirName, null);
		}
		if (subdirs == null) {
			log.info(String.format("%s does not exist or is not al directory",
					baseDirectory));
			throw new RuntimeException("%s does not exist or is not al directory");
			// return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		for (int i = 0; i < subdirs.length; i++) {
			String dirName = subdirs[i];
			if (hostDirs.contains(dirName)) {
				String datafilename = String.format("%s/%s/data.txt", baseDirectory,
						dirName);
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
						Pattern p = Pattern.compile("^(\\S*):  *(.*)$");
						Matcher m = p.matcher(text);
						// reserved for future use
						log.info("scanning for " + key);
						if (m.find()) {
							textKey = m.group(1);
							textValue = m.group(2).trim();
							if (textKey.equalsIgnoreCase(key)) {
								data.put(dirName, textValue);
							}
						}
					}
					reader.close();
					log.info(String.format("datafile %s contents: %s", datafilename,
							contents.toString()));
				} catch (IOException e) {
					// ignore
				}
			}
		}
		return;
	}

	private void formatData(final Map<String, String> data, final String key,
			final List<DataRow> rows) {
		Iterator<String> hostIterator = data.keySet().iterator();
		while (hostIterator.hasNext()) {
			String hostname = hostIterator.next();
			DataRow row = new DataRow();
			rows.add(row.withHostname(hostname).withKey(key)
					.withValue(data.get(hostname)));
		}
		return;
	}

	private void formatData(final Map<String, String> data, final String key,
			final Map<String, HostDataRow> rows) {
		Iterator<String> hostIterator = data.keySet().iterator();
		while (hostIterator.hasNext()) {
			String hostname = hostIterator.next();
			HostDataRow row = new HostDataRow();
			rows.put(hostname, row.withKey(key).withValue(data.get(hostname)));
		}
		return;
	}
}
