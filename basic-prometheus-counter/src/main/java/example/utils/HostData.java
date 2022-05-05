package example.utils;

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

// Class to read host metrics written by legacy monitoring application
public class HostData {
	private boolean debug = false;
	private String hostname = null;
	private List<String> metrics = null;

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

	private Map<String, String> metricTaker = new HashMap<>();

	public void setMetricTaker(Map<String, String> value) {
		metricTaker = value;
	}

	private Path filepPath;
	private Map<String, String> data = new HashMap<>();

	// in a legacy application one has to process the metrics to extract the
	// therefor many columns will be hand crafted
	// numeric values and publish to Prometheus
	public HostData(String hostname) {
		this.hostname = hostname;

		// relies on UNIX soft link making the fixed file path always point to
		// latest results

		// TODO : examine and bail if not a soft link

		filepPath = Paths.get(String.join(System.getProperty("file.separator"),
				Arrays.asList(dataDir, this.hostname, "data.txt")));
	}

	private String dataDir = String.join(System.getProperty("file.separator"),
			Arrays.asList(System.getProperty("user.dir"), "src", "test", "resources",
					"data"));

	public void setDataDir(String value) {
		dataDir = value;
	}

	public HostData() {

	}

	public Map<String, String> getData() {
		return data;
	}

	// read file fully
	public void loadData() {
		try {
			InputStream in = Files.newInputStream(filepPath);
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
		try {
			InputStream in = Files.newInputStream(filepPath);
			BufferedReader bufferedReader = new BufferedReader(
					new InputStreamReader(in));
			String key = null;
			String value = null;

			String line = null;
			while ((line = bufferedReader.readLine()) != null) {
				// collect metrics with non-blank values
				Pattern pattern = Pattern.compile( /* "(?:" to suppress capturing */
						"(" + StringUtils.join(metrics, "|") + ")" + ": " + "([^ ]*)$");
				Matcher matcher = pattern.matcher(line);
				if (matcher.find()) {
					key = matcher.group(1);
					value = matcher.group(2);
					data.put(key, value);
				}

				for (String mKey : metricTaker.keySet()) {
					pattern = Pattern
							.compile("(?:" + mKey + ")" + ": " + metricTaker.get(mKey));
					matcher = pattern.matcher(line);
					if (matcher.find()) {
						key = mKey;
						value = matcher.group(1);
						data.put(key, value);
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
