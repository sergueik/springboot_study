package example.model;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

/*
 *  @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */
// read legacy metrics inventory file
public class HostData {

	private boolean debug = false;
	private String hostname = null;

	private List<Path> filePaths = new ArrayList<>();
	private Map<String, String> data = new HashMap<>();

	private Log logger = LogFactory.getLog(this.getClass());

	public void addFilePath(String dirPath, String filename) {
		final Path filePath = Paths
				.get(String.join(System.getProperty("file.separator"),
						Arrays.asList(dirPath, filename)));
		this.filePaths.add(filePath);
	}

	public HostData(String hostname, String dirPath, String filename) {
		this.hostname = hostname;

		final Path filePath = Paths
				.get(String.join(System.getProperty("file.separator"),
						Arrays.asList(dirPath, filename)));
		this.filePaths.add(filePath);
		// NOTE: without configuration nothing is logged to console
		// System.err.println("HostData intialized with file path: " + filePath);
		// logger.info("HostData intialized with file path: " + filePath);
		// TODO : examine and bail if not a soft link
	}

	public List<String> getFilePaths() {
		return this.filePaths.stream().map((Path filePath) -> filePath.toString())
				.collect(Collectors.toList());
	}

	public List<String> getFileNames() {
		return this.filePaths.stream()
				.map((Path filePath) -> filePath.getFileName().toString())
				.collect(Collectors.toList());
	}
}
