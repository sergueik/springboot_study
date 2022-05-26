package example.service;

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

import org.apache.commons.lang3.StringUtils;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service

// Class to read host metrics written by legacy monitoring application
public class HostData {

	private boolean debug = true;
	private String hostname = null;
	private List<String> metrics = null;

	private static final Logger logger = LogManager.getLogger(HostData.class);

	// NOTE: experiencing failure to @Value annotation to evaluate to data defined
	// in appliction.properties file
	@Value("#{${example.extractedMetricNames}}")
	private Map<String, String> extractedMetricNames;

	public void setExtractedMetricNames(Map<String, String> value) {
		extractedMetricNames = value;
	}

	public boolean isDebug() {
		return debug;
	}

	public void setDebug(boolean value) {
		debug = value;
	}

	public List<String> getMetrics() {
		return metrics;
	}

	public void setMetrics(List<String> value) {
		metrics = value;
	}

	private Map<String, String> metricExtractors = new HashMap<>();

	public void setMetricExtractors(Map<String, String> value) {
		metricExtractors = value;
	}

	private Path filePath;
	private Map<String, String> data = new HashMap<>();

	// in a legacy application one has to process the metrics to extract the
	// therefor many columns will be hand crafted
	// numeric values and publish to Prometheus
	// removing the constuctor
	/*
	public HostData(String hostname) {
		this.hostname = hostname;
	
	}
	*/
	public String getHostname() {
		return hostname;
	}

	public void setHostname(String value) {
		hostname = value;
		// relies on UNIX soft link making the fixed file path always point to
		// latest results

		// TODO : examine and bail if not a soft link

		filePath = Paths.get(String.join(System.getProperty("file.separator"),
				Arrays.asList(dataDir, hostname, "data.txt")));

		data.clear();
	}

	private String dataDir = String.join(System.getProperty("file.separator"),
			Arrays.asList(System.getProperty("user.dir"), "src", "test", "resources",
					"data"));

	public void setDataDir(String value) {
		dataDir = value;
	}

	public HostData() {
		if (debug)
			if (extractedMetricNames != null) {
				logger.info("Known metric names: " + extractedMetricNames.keySet());
			} else {
				logger.info("No known metric names loaded");
			}

	}

	public Map<String, String> getData() {
		return data;
	}

	// read file fully
	public void loadData() {
		try {
			InputStream in = Files.newInputStream(filePath);
			BufferedReader bufferedReader = new BufferedReader(
					new InputStreamReader(in));
			String key = null;
			String value = null;

			String line = null;
			while ((line = bufferedReader.readLine()) != null) {
				// collect metrics with non-blank values
				Pattern pattern = Pattern.compile("^(.*):  *(.*)$");
				Matcher matcher = pattern.matcher(line);
				if (matcher.find()) {
					key = matcher.group(1);
					value = matcher.group(2);
					data.put(key, value);
				}
			}
			bufferedReader.close();

		} catch (FileNotFoundException e) {
			System.err.println("Exception (ignored) " + e.toString());
		} catch (IOException e) {
			System.err.println("Exception (ignored) " + e.toString());
		}
	}

	public void readData() {
		String key = null;
		String value = null;
		String line = null;
		try {
			InputStream in = Files.newInputStream(filePath);
			BufferedReader bufferedReader = new BufferedReader(
					new InputStreamReader(in));

			while ((line = bufferedReader.readLine()) != null) {

				key = null;
				value = null;
				// collect metrics with non-blank values
				Pattern pattern = Pattern.compile( /* "(?:" to suppress capturing */
						"(" + StringUtils.join(metrics, "|") + ")" + ": " + "([^ ]*)$");
				Matcher matcher = pattern.matcher(line);
				if (matcher.find()) {
					key = matcher.group(1);
					value = matcher.group(2);
				}
				// NOTE: duplicate variable
				for (String mKey : metricExtractors.keySet()) {
					if (debug)
						logger.info(String.format("processing metric extractor: %s %s",
								mKey, metricExtractors.get(mKey)));
					pattern = Pattern.compile(
							"^\\s*(?:" + mKey + ")" + ": " + metricExtractors.get(mKey));
					matcher = pattern.matcher(line);
					if (matcher.find()) {
						key = mKey;
						value = matcher.group(1);
						if (debug)
							logger.info(
									String.format("Found data for metric %s: %s", key, value));
					}
					// NOTE: hack
					if (value != null) {
						String realKey = extractedMetricNames != null
								&& extractedMetricNames.containsKey(key)
										? extractedMetricNames.get(key) : key;
						if (debug)
							logger.info(String.format("Adding data for metric %s(%s): %s",
									key, realKey, value));
						data.put(realKey, value);
					}
				}
			}
			bufferedReader.close();

		} catch (FileNotFoundException e) {
			System.err.println("Exception (ignored) " + e.toString());
		} catch (IOException e) {
			System.err.println("Exception (ignored) " + e.toString());
		}
	}
}
