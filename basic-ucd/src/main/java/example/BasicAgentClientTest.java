package example;

/**
 * Copyright 2020 Serguei Kouzmine
 */
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.urbancode.ud.client.AgentClient;
import com.urbancode.ud.client.ComponentClient;
import com.urbancode.ud.client.ResourceClient;

/*
 * This example exercises
 * ResourceClient.getResourceByPath
 * ResourceClient.getResourceChildren
 * ComponentClient.getComponentUUID
 * file:///C:/developer/sergueik/springboot_study/basic-ucd/uDeployRestClient/docs/index.html
 */
public class BasicAgentClientTest {
	private static final List<String> fields = Arrays.asList("id", "name",
			"description");
	private static final List<String> fields2 = Arrays.asList("name",
			"description", "value");
	private static final List<String> fields3 = Arrays.asList("name", "path",
			"description");

	private static boolean debug = false;
	private static boolean verbose = false;

	private static String user;
	private static String password;
	private static String server;
	private static String path;
	private static String id;
	private static String env = null;

	private static CommandLineParser commandLineParser;
	private static ResourceClient resourceClient;
	private static ComponentClient componentClient;
	private static AgentClient agentClient;

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {
		commandLineParser = new CommandLineParser();
		commandLineParser.flagsWithValues.add("user");
		commandLineParser.flagsWithValues.add("password");
		commandLineParser.saveFlagValue("server");
		commandLineParser.saveFlagValue("path");
		commandLineParser.saveFlagValue("id");
		commandLineParser.saveFlagValue("env");

		commandLineParser.parse(args);

		if (commandLineParser.flags.containsKey("debug")) {
			debug = true;
		}
		if (commandLineParser.flags.containsKey("verbose")) {
			verbose = true;
		}
		// static inner allows accessing private members from enclosing class
		// directly
		user = commandLineParser.flags.get("user");
		if (user == null) {
			user = "admin";
			System.err.println("Missing argument: user - using default");
		}
		password = commandLineParser.flags.get("password");
		if (password == null) {
			password = "admin";
			System.err.println("Missing argument: password - using default");
		}
		server = commandLineParser.getFlagValue("server");
		if (server == null) {
			server = "https://localhost:8443";
			System.err.println("Missing argument: server - using default");
		}

		// TODO: get resource id legitimately
		path = commandLineParser.getFlagValue("path");
		id = commandLineParser.getFlagValue("id");
		if ((id == null) && (path == null)) {
			System.err.println("Missing required argument: path or id");
			return;
		}

		// explore resource hierarchy

		componentClient = new ComponentClient(new URI(server), user, password);
		agentClient = new AgentClient(new URI(server), user, password);
		resourceClient = new ResourceClient(new URI(server), user, password);

		if (resourceClient == null || agentClient == null
				|| componentClient == null) {
			throw new RuntimeException(String.format(
					"failed to connect to server %s as user: %s / password: %s", server,
					user, password));
		}

		// JSONObject data = (id != null) ? resourceClient.getResourceById(id) :
		// resourceClient.getResourceByPath(path);
		// env = data.getString("id");
		env = (id != null) ? id
				: resourceClient.getResourceByPath(path).getString("id");
		JSONArray resourceChildrenJsonArray = resourceClient
				.getResourceChildren(env);
		if (debug) {
			System.out
					.println("{\"" + env + "\": " + resourceChildrenJsonArray + " }");
		}
		for (int index = 0; index != resourceChildrenJsonArray.length(); index++) {
			JSONObject resourceChildObject = resourceChildrenJsonArray
					.getJSONObject(index);

			System.out.println("  - ");
			for (String field : fields) {
				if (resourceChildObject.getString(field) != null
						&& resourceChildObject.getString(field) != "") {
					System.out.println(String.format("  %s: \"%s\"", field,
							resourceChildObject.getString(field)));
				}
			}
			String resourceChildId = resourceChildObject.getString("id");

			JSONArray resourceGrandChildrenJsonArray = resourceClient
					.getResourceChildren(resourceChildId);
			if (verbose) {
				System.out.println("{\"" + resourceChildId + "\": "
						+ resourceGrandChildrenJsonArray + " }");
			}
			for (int index1 = 0; index1 != resourceGrandChildrenJsonArray
					.length(); index1++) {
				JSONObject resourceGrandChild = resourceGrandChildrenJsonArray
						.getJSONObject(index1);
				System.out.println("    - ");
				for (String field : fields) {
					try {
						if (resourceGrandChild.getString(field) != null
								&& resourceGrandChild.getString(field) != "") {
							System.out.println(String.format("    %s: \"%s\"", field,
									resourceGrandChild.getString(field)));
						}
					} catch (JSONException e) {
						// totally ignore for now
					}
				}

				String componentName = resourceGrandChild.getString("name");
				// System.out.println("Examine component name:" + componentName);

				// getResourceProperty
				try {
					UUID componentUUID = componentClient.getComponentUUID(componentName);
					JSONObject componentObject = componentClient
							.getComponent(componentUUID.toString());
					JSONObject resourceRole = componentObject
							.getJSONObject("resourceRole");
					JSONArray propDefsArray = resourceRole.getJSONArray("propDefs");
					for (int index2 = 0; index2 != propDefsArray.length(); index2++) {
						JSONObject propertyObject = propDefsArray.getJSONObject(index2);
						System.out.println("      # property definitions");
						System.out.println("      -");
						for (String field3 : fields2) {
							if (propertyObject.getString(field3) != null
									&& propertyObject.getString(field3) != "") {
								System.out.println(String.format("      %s: \"%s\"", field3,
										propertyObject.getString(field3)));
							}
						}
					}
				} catch (IOException e) {
					// print information and continue
					System.out.println("Exception during examine component "
							+ componentName + " (ignored) " + e.toString());
					if (debug) {
						int spacesToIndentEachLevel = 2;
						System.out.println("Defective object:\n"
								+ new JSONObject(resourceChildObject.toString())
										.toString(spacesToIndentEachLevel));
					} else {
						for (String field4 : fields3) {
							if (resourceChildObject.getString(field4) != null
									&& resourceChildObject.getString(field4) != "") {
								System.out.println(String.format("      %s: \"%s\"", field4,
										resourceChildObject.getString(field4)));
							}
						}
					}
				}
			}
		}
	}

	private static class CommandLineParser {

		private boolean debug = false;

		public boolean isDebug() {
			return debug;
		}

		public void setDebug(boolean debug) {
			this.debug = debug;
		}

		// pass-through
		private String[] arguments = null;

		public String[] getArguments() {
			return arguments;
		}

		private Map<String, String> flags = new HashMap<>();

		//
		// the flag values that are expected to be followed with a value
		// that allows the application to process the flag.
		//
		private Set<String> flagsWithValues = new HashSet<>();

		public Set<String> getFlags() {
			Set<String> result = flags.keySet();
			return result;
		}

		public String getFlagValue(String flagName) {
			return flags.get(flagName);
		}

		public int getNumberOfArguments() {
			return arguments.length;
		}

		public int getNumberOfFlags() {
			return flags.size();
		}

		public boolean hasFlag(String flagName) {
			return flags.containsKey(flagName);
		}

		// contains no constructor nor logic to discover unknown flags
		public void parse(String[] args) {
			List<String> regularArgs = new ArrayList<>();

			for (int n = 0; n < args.length; ++n) {
				if (args[n].charAt(0) == '-') {
					String name = args[n].replaceFirst("-", "");
					String value = null;
					// remove the dash
					if (debug) {
						System.err.println("Examine: " + name);
					}
					if (flagsWithValues.contains(name) && n < args.length - 1
							&& !args[n + 1].matches("^-")) {
						String data = args[++n];
						// https://www.baeldung.com/java-case-insensitive-string-matching
						value = data.matches("(?i)^env:[a-z_0-9]+")
								? System.getenv(data.replaceFirst("(?i)^env:", "")) : data;

						if (debug) {
							if (data.matches("(?i)^env:[a-z_0-9]+")) {
								System.err
										.println("Evaluate value for: " + name + " = " + value);

							} else {
								System.err
										.println("Collect value for: " + name + " = " + value);
							}
						}
					} else {
						if (debug) {
							System.err.println("Ignore the value for " + name);
						}
					}
					flags.put(name, value);
				}

				else
					regularArgs.add(args[n]);
			}

			arguments = regularArgs.toArray(new String[regularArgs.size()]);
		}

		public void saveFlagValue(String flagName) {
			flagsWithValues.add(flagName);
		}

		private static final String keyValueSeparator = ":";
		private static final String entrySeparator = ",";

		// Example data:
		// -argument "{count:0, type:navigate, size:100, flag:true}"
		// NOTE: not using org.json to reduce size
		public Map<String, String> extractExtraArgs(String argument)
				throws IllegalArgumentException {

			final Map<String, String> extraArgData = new HashMap<>();
			argument = argument.trim().substring(1, argument.length() - 1);
			if (argument.indexOf("{") > -1 || argument.indexOf("}") > -1) {
				if (debug) {
					System.err.println("Found invalid nested data");
				}
				throw new IllegalArgumentException(
						"Nested JSON athuments not supprted");
			}
			final String[] pairs = argument.split(entrySeparator);

			for (String pair : pairs) {
				String[] values = pair.split(keyValueSeparator);

				if (debug) {
					System.err.println("Collecting: " + pair);
				}
				extraArgData.put(values[0].trim(), values[1].trim());
			}
			return extraArgData;
		}

	}
}
