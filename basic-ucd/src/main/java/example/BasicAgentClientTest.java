package example;

// import static org.junit.Assert.assertEquals;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.apache.commons.codec.EncoderException;
import com.urbancode.ud.client.AgentClient;
import com.urbancode.ud.client.ResourceClient;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;
import org.codehaus.jettison.json.JSONArray;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class BasicAgentClientTest {
	private static boolean debug = false;
	private static CommandLineParser commandLineParser;

	private static AgentClient client;
	private static JSONObject data;

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {
		commandLineParser = new CommandLineParser();
		commandLineParser.flagsWithValues.add("user");
		commandLineParser.flagsWithValues.add("password");
		commandLineParser.saveFlagValue("agent");

		commandLineParser.parse(args);

		if (commandLineParser.flags.containsKey("debug")) {
			debug = true;
		}
		// static inner allows accessing private members from enclosing class
		// directly
		String user = commandLineParser.flags.get("user");
		if (user == null) {
			System.err.println("Missing required argument: user - assuming default");
			user = "admin";
			// return;
		}
		String password = commandLineParser.flags.get("password");
		if (password == null) {
			System.err
					.println("Missing required argument: password - assuming default");
			password = "admin";
			// return;
		}

		String agent = commandLineParser.getFlagValue("agent");
		if (agent == null) {
			System.err.println("Missing required argument: agent");
			return;
		}
		client = new AgentClient(new URI("https://localhost:8443"), user, password);
		if (client == null) {
			throw new RuntimeException(String
					.format("failed to connect as %s / password %s", user, password));
		} else {
			data = client.getAgent(agent);
			// System.out.println(data);
		}
		// explore resource hierarchy
		ResourceClient r = new ResourceClient(new URI("https://localhost:8443"),
				user, password);
		// TODO: get id
		String id = "172ecdb3-50a2-e489-6b50-1399b396b6fb";
		JSONArray jsonArray = r.getResourceChildren(id);
		// System.out.println("{\"" + id + "\": "+ jsonArray + " }");

		for (int index = 0; index != jsonArray.length(); index++) {
			JSONObject childObject = jsonArray.getJSONObject(index);
			String id1 = childObject.getString("id");
			JSONArray ce1 = r.getResourceChildren(id1);
			// String id1 = "172ecdb9-54f7-c269-9cca-fd8bd9ee6341";
			System.out.println("{\"" + id1 + "\": " + ce1 + " }");
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
					if (flagsWithValues.contains(name) && n < args.length - 1) {
						value = args[++n];
						if (debug) {
							System.err.println("Collect value for: " + name + " = " + value);
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

