package example.controller;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.io.BufferedReader;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.lang.IllegalArgumentException;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.factory.annotation.Autowired;
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
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;

import example.model.Artist;
import example.model.HostData;
import java.io.FileWriter;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.time.Instant;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import example.model.ArtistSerializer;

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

}
