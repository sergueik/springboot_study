package example;

/**
 * Copyright 2020 Serguei Kouzmine
 */
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

/*
 * This example exercises browsing versioned properties (unfinished)
 * */
public class GetResourceRoleProperties {
	private static final List<String> fields = Arrays.asList("id", "name",
			"description");
	private static final List<String> fields2 = Arrays.asList("name",
			"description", "value");
	private static final List<String> fields3 = Arrays.asList("name", "path",
			"description");

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
		commandLineParser.saveFlagValue("user");
		commandLineParser.saveFlagValue("password");
		commandLineParser.saveFlagValue("server");
		commandLineParser.saveFlagValue("agent");
		commandLineParser.saveFlagValue("env");

		commandLineParser.parse(args);

		if (commandLineParser.hasFlag("debug")) {
			debug = true;
		}
		if (commandLineParser.hasFlag("verbose")) {
			verbose = true;
		}
		// static inner allows accessing private members from enclosing class
		// directly
		String user = commandLineParser.getFlagValue("user");
		if (user == null) {
			user = "admin";
			System.err.println("Missing argument: user - using default");
		}
		String password = commandLineParser.getFlagValue("password");
		if (password == null) {
			password = "admin";
			System.err.println("Missing argument: password - using default");
		}
		String agent = commandLineParser.getFlagValue("agent");
		if (agent == null) {
			agent = "new agent";
			System.err.println("Missing argument: agent - using default");
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
		// TODO: switch to agentClient.
		JSONObject resourceJSONObject = resourceClient.getResourceRoleByName(agent);
		String id3 = "172f1d3c-3665-6fd2-1c71-f0e67ec633c5";
		System.out.println(
				"Calling for " + id3 + " " + resourceJSONObject.getString("id"));
		JSONArray xxx = resourceClient.getResourceRoleProperties(id3,
				resourceJSONObject.getString("id"));
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
