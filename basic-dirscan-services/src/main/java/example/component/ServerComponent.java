package example.component;

/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

@Component
public class ServerComponent {

	private List<String> servers = null;
	private Set<String> includeFilenames = new HashSet<>();
	private String baseDirectory = System.getProperty("os.name").toLowerCase()
			.contains("windows") ? System.getenv("TEMP") : "/tmp";

	@Value("${example.ServerComponent.debug:false}")
	private boolean debug = false;

	public void setDebug(boolean data) {
		debug = data;
	}

	public ServerComponent() {
	}

	public List<String> getServers() {
		return servers;
	}

	public ServerComponent(String configFile) {
		servers = new ArrayList<>();
		if (!new File(String.format("%s/%s", baseDirectory, configFile)).exists()) {
			throw new IllegalArgumentException("inaccessible file: " + configFile);
		}
		getConfig(configFile, servers);
	}

	public void getConfig(String filename, List<String> data)
			throws RuntimeException {
		includeFilenames.add(filename);
		File file = null;
		List<String> lines = new ArrayList<>();
		BufferedReader reader = null;
		StringBuffer contents = new StringBuffer();
		String text = null;
		try {
			file = new File(String.format("%s/%s", baseDirectory, filename));
			reader = new BufferedReader(new FileReader(file));
			while ((text = reader.readLine()) != null) {
				contents.append(text).append(System.getProperty("line.separator"));
				lines.add(text.trim());
			}
			reader.close();
		} catch (IOException e) {
			throw new RuntimeException("Exception: " + e.getMessage());
		}
		if (debug)
			System.err.println("Loaded contents: " + contents);
		for (int cnt = 0; cnt != lines.size(); cnt++) {
			String line = lines.get(cnt);
			if (line.matches("^# ") || line.isEmpty())
				continue;
			if (line.matches("#include (?:\\w+) *$")) {
				Pattern pattern = Pattern.compile("#include (\\w+) *$",
						Pattern.CASE_INSENSITIVE);
				Matcher matcher = pattern.matcher(line);
				if (matcher.find()) {
					String includedFilename = matcher.group(1);
					if (includeFilenames.contains(includedFilename))
						throw new IllegalArgumentException(
								"cyclic #include detected in line " + line);
					getConfig(includedFilename, data);
				}
			} else if (line.matches("#exec (?:[^ ]*) *$")) {
				continue;
			} else {
				data.add(line);
				if (debug)
					System.err.println("Adding line: " + line);
			}

		}
	}

}
