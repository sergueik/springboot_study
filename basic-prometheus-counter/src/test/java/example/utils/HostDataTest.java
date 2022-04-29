package example.utils;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;

import java.nio.file.Files;
import java.nio.file.Paths;

import java.lang.reflect.Field;
import java.lang.reflect.Type;

import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.UUID;

import java.util.regex.MatchResult;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.jupiter.api.Test;
import org.xml.sax.SAXException;
import org.yaml.snakeyaml.Yaml;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

public class HostDataTest {
	private HostData hostData = null;
	private List<String> metrics = new ArrayList<>();
	private Map<String, String> data = new HashMap<>();

	private String hostname = "hostname00";

	@Test
	public void test() throws Exception {
		hostData = new HostData(hostname);
		metrics.add("cpu");
		metrics.add("memory");
		metrics.add("rpm");
		metrics.add("disk");
		metrics.add("missing data");
		metrics.add("load_average");
		hostData.setMetrics(metrics);
		hostData.readData();
		data = hostData.getData();

		assertThat(data, notNullValue());
		assertThat(data.keySet().size(), is(5));
		// NOTE: loading all metrics as strings
		assertThat(data.get("memory"), is("20"));
		assertThat(data.containsKey("missing data"), is(false));
		assertThat(data.containsKey("dummy"), is(false));
	}
}
