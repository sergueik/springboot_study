package example;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.apache.commons.codec.EncoderException;
import com.urbancode.ud.client.AgentClient;
import com.urbancode.ud.client.ResourceClient;
import com.urbancode.ud.client.ComponentClient;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.codehaus.jettison.json.JSONArray;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

public class BasicAgentClientTest {
	private static final List<String> fields = Arrays.asList("id", "name",
			"description");
	private static final List<String> fields2 = Arrays.asList("name",
			"description", "value");

	private static boolean debug = false;
	private static boolean verbose = false;

	private static CommandLineParser commandLineParser;
	private static ResourceClient resourceClient;
	private static ComponentClient componentClient;
	private static AgentClient agentClient;
	private static JSONObject data;

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {
		commandLineParser = new CommandLineParser();
		commandLineParser.flagsWithValues.add("user");
		commandLineParser.flagsWithValues.add("password");
		commandLineParser.saveFlagValue("server");
		commandLineParser.saveFlagValue("newname");
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
		String user = commandLineParser.flags.get("user");
		if (user == null) {
			user = "admin";
			System.err.println("Missing argument: user - using default");
		}
		String password = commandLineParser.flags.get("password");
		if (password == null) {
			password = "admin";
			System.err.println("Missing argument: password - using default");
		}
		String newname = commandLineParser.flags.get("newname");
		if (newname == null) {
			newname = "brand new agent";
			System.err.println("Missing argument: newname - using default");
		}
		String server = commandLineParser.getFlagValue("server");
		if (server == null) {
			server = "https://localhost:8443";
			System.err.println("Missing argument: server - using default");
		}
		// TODO: get env id legitimately
		String env = commandLineParser.getFlagValue("env");
		if (env == null) {
			System.err.println("Missing required argument: env");
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
		// String resourceName = "TEST";
		// JSONObject resourceJSONObject = resourceClient
		// .getResourceRoleByName(resourceName);
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
					if (resourceGrandChild.getString(field) != null
							&& resourceGrandChild.getString(field) != "") {
						System.out.println(String.format("    %s: \"%s\"", field,
								resourceGrandChild.getString(field)));
					}
				}

				String componentName = resourceGrandChild.getString("name");
				// System.out.println("Examine component name:" + componentName);

				// getResourceProperty
				UUID componentUUID = componentClient.getComponentUUID(componentName);
				// System.out.println("Converted to component uuid:" + componentUUID);
				// TODO: how to get it
				/// id2 = "172f1d3c-35f2-7aa1-a9a3-d2fb56513b79";
				// Component Properties, not what we need
				/*
				Map<String, String> propMap = componentClient
						.getComponentProperties(componentUUID.toString());
				for (String propName : propMap.keySet()) {
					System.out
							.println(String.format("%s=%s", propName, propMap.get(propName)));
				}
				*/
				/*
				JSONObject data = componentClient
						.getComponentVersionPropSheetDef(componentUUID.toString());
				System.out.println("Component prop sheet def:" + data);
				*/
				JSONObject componentObject = componentClient
						.getComponent(componentUUID.toString());
				JSONObject resourceRole = componentObject.getJSONObject("resourceRole");
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

				String id3 = "172f1d3c-3665-6fd2-1c71-f0e67ec633c5";
				System.out.println(
						"Calling for " + id3 + " " + resourceGrandChild.getString("id"));
				JSONArray xxx = resourceClient.getResourceRoleProperties(id3,
						resourceGrandChild.getString("id"));
				JSONObject yyy = xxx.getJSONObject(0).getJSONObject("propValue");
				System.out.println(yyy);
				// need to call
				// curl
				// 'https://192.168.0.64:8443/rest/resource/resource/1730c2d2-6a32-aff4-f7a1-9915402a8a58/propertiesForRole/172f1d3c-3665-6fd2-1c71-f0e67ec633c5?rowsPerPage=10&pageNumber=1&sortType=asc'
				// \
				// System.out.println("Component objects:" + data);
				/*
				resourceClient.getResourceProperty(childObject1.getString("id"),
						"test_property");
				*/
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
