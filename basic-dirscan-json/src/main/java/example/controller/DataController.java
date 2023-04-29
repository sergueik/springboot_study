package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.OpenOption;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.stream.JsonReader;

import com.google.gson.JsonElement;

import example.model.Artist;
import example.model.ArtistSerializer;
import example.model.HostData;

import java.io.IOException;
import java.nio.file.FileAlreadyExistsException;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.FileAttribute;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;
import java.io.File;
import java.io.IOException;

@RestController
@RequestMapping("/")
public class DataController {

	private static Gson gson = new GsonBuilder()
			.registerTypeAdapter(Artist.class, new ArtistSerializer()).create();

	private static Map<String, String> data = new HashMap<>();
	private static List<Map<String, String>> metricsData = new ArrayList<>();
	private static Log log = LogFactory.getLog(DataController.class);
	private static boolean verifylinks = false;
	private static String linkedDataDir = null;
	private static HostData hostData = null;
	private static final String filemask = "*.json$";
	private String baseDirectory = System.getProperty("os.name").toLowerCase()
			.contains("windows") ? System.getenv("TEMP") : "/tmp";
	static int cnt = 100;
	private static boolean debug = false;
	private static Map<String, HostData> inventory = new HashMap<>();

	@ResponseBody
	@GetMapping("/file/{name}")
	public ResponseEntity<List<Map<String, String>>> showFile(
			@PathVariable("name") String name) throws IOException {

		List<String> hostDirs = new ArrayList<>();
		Map<String, String> data = new HashMap<>();

		try {
			log.info(String.format("Read hosts list from %s", name));
			metricsData = listFilesDsNames(name);
			return ResponseEntity.status(HttpStatus.OK)
					.contentType(MediaType.APPLICATION_JSON_UTF8).body(metricsData);
		} catch (RuntimeException e) {
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
	}

	@ResponseBody
	@GetMapping("/data/{name}")
	public ResponseEntity<String> showData(@PathVariable("name") String name)
			throws IOException {

		ArtistSerializer serializer = new ArtistSerializer();

		Artist artist = new Artist(1, name, "instrument");

		JsonElement rowJson = serializer.serialize(artist, null, null);
		return ResponseEntity.status(HttpStatus.OK)
				.contentType(MediaType.APPLICATION_JSON_UTF8).body(rowJson.toString());

	}

	@ResponseBody
	@GetMapping("/databroken/{name}")
	public ResponseEntity<JsonElement> showDataBroken(
			@PathVariable("name") String name) throws IOException {

		ArtistSerializer serializer = new ArtistSerializer();

		Artist artist = new Artist(1, name, "instrument");
		artist.setPrice((float) 2.5); // will not be printed

		JsonElement rowJson = serializer.serialize(artist, null, null);
		// ExceptionResolver : Failed to write HTTP message:
		// org.springframework.http.converter.HttpMessageNotWritableException: Could
		// not write JSON: Not a JSON Primitive: {"id":1,"plays":"instrument"};
		// nested exception is com.fasterxml.jackson.databind.JsonMappingException:
		// Not a JSON Primitive: {"id":1,"plays":"instrument"} (through reference
		// chain: com.google.gson.JsonObject["asJsonPrimitive"])
		return ResponseEntity.status(HttpStatus.OK)
				.contentType(MediaType.APPLICATION_JSON_UTF8).body(rowJson);

	}

	private final static String defaulthost = "localhost";

	@GetMapping(value = "/listfilenames/{hostname}", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public List<String> arrayFileNames(@PathVariable String hostname) {

		HostData hostdata = new HostData(hostname, System.getProperty("user.dir"),
				"dummy1.txt");
		hostdata.addFilePath(System.getProperty("user.dir"), "dummy2.txt");

		inventory.put(defaulthost, hostdata);
		final List<String> results = (inventory.containsKey(hostname))
				? inventory.get(hostname).getFileNames() : new ArrayList<>();

		return results;
	}

	// NOTE: without content-type header will receive:
	// curl -d '{"name": "x", "plays": "drums", "id":1}' -XPOST
	// http://localhost:8080/updatedata/xxx
	// Controller : payload:
	// %7B%22name%22%3A+%22x%22%2C+%22plays%22%3A+%22drums%22%2C+%22id%22%3A1%7D=
	@PostMapping(value = "/updatedata/{name}", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public ResponseEntity<String> updateData(@PathVariable String name,
			@RequestBody String payload) {
		log.info("RequestBody(raw): " + payload);
		Artist artist = gson.fromJson(payload, Artist.class);
		log.info("RequestBody(parsed): " + gson.toJson(artist));
		JsonReader reader = new JsonReader(
				new InputStreamReader(new ByteArrayInputStream(payload.getBytes())));
		artist = gson.fromJson(reader, Artist.class);
		log.info("RequestBody(parsed, 2 nd attempt): " + gson.toJson(artist));

		// origin:
		// http://www.java2s.com/Tutorials/Java/java.nio.file/Files/Java_Files_createFile_Path_path_FileAttribute_lt_gt_attrs_.htm
		// https://www.javatpoint.com/how-to-create-a-file-in-java
		// see also:
		// https://stackoverflow.com/questions/74608272/java-files-existspath-fails-on-linux-but-passes-on-windows
		createFile(name);
		try {
			Files.write(Paths.get("C:\\temp\\" + name), payload.getBytes(),
					StandardOpenOption.WRITE);
		} catch (IOException e) {
			log.info("Exception wrigint file:" + e.toString());

		}
		return (artist.getName() == null || name == null
				|| !name.equals(artist.getName()))
						? ResponseEntity.status(HttpStatus.BAD_REQUEST).body("bad name")
						: ResponseEntity.status(HttpStatus.OK)
								.contentType(MediaType.APPLICATION_JSON_UTF8)
								.body("{\"status\":\"OK\"}");
	}

	@GetMapping(value = "/listdata/{hostname}", produces = {
			MediaType.APPLICATION_JSON_VALUE })
	public List<String> arrayFilePaths(@PathVariable String hostname) {

		HostData hostdata = new HostData(hostname, System.getProperty("user.dir"),
				"dummy1.txt");
		hostdata.addFilePath(System.getProperty("user.dir"), "dummy2.txt");

		inventory.put(defaulthost, hostdata);
		final List<String> results = (inventory.containsKey(hostname))
				? inventory.get(hostname).getFilePaths() : new ArrayList<>();

		return results;
	}

	private static List<Map<String, String>> readFiles(List<Path> result) {
		List<Map<String, String>> results = new ArrayList<>();
		System.err.println(String.format("Ingesting %d files: ", result.size()));
		result.stream().forEach(o -> {
			hostData = new HostData("dummy",
					o.getParent().toAbsolutePath().toString(),
					o.getFileName().toString());

			File file = null;
			BufferedReader reader = null;
			StringBuffer contents = new StringBuffer();
			String text = null;
			try {
				file = new File(o.getFileName().toString());
				reader = new BufferedReader(new FileReader(file));
				while ((text = reader.readLine()) != null) {
					contents.append(text).append(System.getProperty("line.separator"));
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
			data.put(o.getFileName().toString(), contents.toString());
			results.add(data);
			// sync debug settings

		});
		return results;

	}

	// NOTE: not reducing to calling the other method with a empty argument
	// return listFilesDsNames(
	// path,
	// new ArrayList<String>(),
	// new ArrayList<String>());
	private static List<Map<String, String>> listFilesDsNames(String path)
			throws IOException {

		Path basePath = Paths.get(path);
		// NOTE: do not use File.separator
		final String basePathUri = new URL(
				getDataFileUri(basePath.toAbsolutePath().toString())).getFile() + "/";
		System.err.println("Scanning path: " + basePathUri);
		// origin:
		// https://github.com/mkyong/core-java/blob/master/java-io/src/main/java/com/mkyong/io/api/FilesWalkExample.java
		// sub-optimal
		List<Path> result;
		List<Path> result2;
		//
		if (verifylinks) {
			try (Stream<Path> walk = Files.walk(basePath)) {
				result2 = walk.filter(Files::isSymbolicLink).filter(o -> {
					try {
						Path targetPath = Files.readSymbolicLink(o.toAbsolutePath());
						if (debug)
							System.err.println("Testing link " + o.getFileName().toString()
									+ " target path " + targetPath.toString());

						File target = new File(
								String.format(
										"%s/%s", (linkedDataDir == null
												? o.getParent().toAbsolutePath() : linkedDataDir),
										targetPath.toString()));
						if (target.exists() && target.isFile())
							if (debug)
								System.err.println("Valid link " + o.getFileName().toString()
										+ " target path " + target.getCanonicalPath());
						return true;

					} catch (IOException e) {
						// fall through
					}

					return false;
				}).filter(o -> o.getFileName().toString().matches(filemask))
						.collect(Collectors.toList());
			}
			return readFiles(result2);
		} else {
			try (Stream<Path> walk = Files.walk(basePath)) {
				result = walk.filter(Files::isRegularFile)
						.filter(o -> o.getFileName().toString().matches(filemask))
						.collect(Collectors.toList());
			}
			// NOTE: streams are not designed to be rescanned
			return readFiles(result);
		}

	}

	private static String getDataFileUri(String dataFilePath) {
		return osName.equals("windows")
				? "file:///" + dataFilePath.replaceAll("\\\\", "/")
				: "file://" + dataFilePath;
	}

	private static String osName = getOSName();

	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

	public static void createFile(String name) {
		File file = new File("C:\\temp\\" + name); // initialize File object and
																								// passing path as argument
		boolean result;
		try {
			result = file.createNewFile(); // creates a new file
			if (result) // test if successfully created a new file
			{
				log.info("file created " + file.getCanonicalPath()); // returns
																															// the
																															// path
																															// string
			} else {
				log.info("File already exist at location: " + file.getCanonicalPath());
			}
		} catch (IOException e) {
			log.info(e.toString());
			// prints exception if any
		}
	}
}
