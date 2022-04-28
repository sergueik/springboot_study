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

public class SnakeYamlReaderTest {

	Map<String, Host> info = new HashMap<>();
	String fileName = "cluster.yaml";
	String encoding = "UTF-8";

	@Test
	public void test() throws Exception {
		try {
			InputStream in = Files.newInputStream(
					Paths.get(String.join(System.getProperty("file.separator"),
							Arrays.asList(System.getProperty("user.dir"), "src", "test",
									"resources", fileName))));
			@SuppressWarnings("unchecked")
			ArrayList<LinkedHashMap<Object, Object>> members = (ArrayList<LinkedHashMap<Object, Object>>) new Yaml()
					.load(in);
			for (LinkedHashMap<Object, Object> row : members) {
				// NOTE: YAML may be confused by formating like "08" and assume double
				int id = (int) row.get("id");
				Host host = new Host(id, (String) row.get("hostname"),
						(String) row.get("dc"), (String) row.get("app"));
				@SuppressWarnings("unused")
				String hostname = host.getHostname();
				// filter what to (not) serialize

				String app = host.getApp();
				if (app != null && !app.isEmpty()) {
					info.put(hostname, host);
				}
			}
			System.err.println(info.keySet());
		} catch (IOException e) {
			System.err.println("Excption (ignored) " + e.toString());
		}
	}

	public static class Host {

		private String hostname;
		private String app;
		private String dc;
		private static String staticInfo;
		private int id;

		public String getHostname() {
			return hostname;
		}

		public void setHostname(String data) {
			hostname = data;
		}

		public String getApp() {
			return app;
		}

		public void setApp(String data) {
			app = data;
		}

		public String getDc() {
			return dc;
		}

		public void setDc(String data) {
			dc = data;
		}

		public int getId() {
			return id;
		}

		public void setId(int data) {
			id = data;
		}

		public Host() {
			staticInfo = UUID.randomUUID().toString();
		}

		public /* static */ String getStaticInfo() {
			return Host.staticInfo;
		}

		public Host(int id, String hostname, String dc, String app) {
			super();
			if (Host.staticInfo == null) {
				Host.staticInfo = UUID.randomUUID().toString();
			}
			this.hostname = hostname;
			this.id = id;
			this.dc = dc;
			this.app = app;
		}

	}

}
