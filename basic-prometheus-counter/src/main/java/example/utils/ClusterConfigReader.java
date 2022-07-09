package example.utils;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.UUID;

import org.yaml.snakeyaml.DumperOptions;
import org.yaml.snakeyaml.DumperOptions.FlowStyle;
import org.yaml.snakeyaml.LoaderOptions;
import org.yaml.snakeyaml.representer.Representer;

import example.entity.Host;

import org.yaml.snakeyaml.Yaml;
import org.yaml.snakeyaml.constructor.BaseConstructor;

public class ClusterConfigReader {
	private Map<String, Host> info = new HashMap<>();

	public Map<String, Host> getInfo() {
		return info;
	}

	public void read(String fileName) {
		Path filepPath = Paths.get(String.join(System.getProperty("file.separator"),
				Arrays.asList(System.getProperty("user.dir"), "src", "test",
						"resources", fileName)));
		try {
			InputStream in = Files.newInputStream(filepPath);
			@SuppressWarnings("unchecked")
			ArrayList<LinkedHashMap<Object, Object>> members = (ArrayList<LinkedHashMap<Object, Object>>) new Yaml()
					.load(in);
			for (LinkedHashMap<Object, Object> row : members) {
				// NOTE: YAML may be confused by formating like "08" and assume double
				int id = (int) row.get("id");
				Host host = new Host(id, (String) row.get("hostname"),
						(String) row.get("datacenter"), (String) row.get("environment"),
						(String) row.get("appid"));
				String hostname = host.getHostname();

				// inspect app property to filter what to (not) serialize

				String appid = host.getAppid();
				if (appid != null && !appid.isEmpty()) {
					info.put(hostname, host);
				}
			}
			System.err.println(info.keySet());
		} catch (IOException e) {
			System.err.println("Exception (ignored) " + e.toString());
		}
	}
}
