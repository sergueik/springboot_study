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
 * This example exercises browsing UCD resource tree from top level group to agent and components
 * ResourceClient.getResourceChildren
 * and
 * ComponentClient.getComponentUUID
 * ComponentClient.getComponent
 * it contains embedded static CommandLineParser class (for ease of deployment)
 * */
public class GetResourceRoleByName {
	private static final List<String> fields = Arrays.asList("id", "name", "path",
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
		String server = commandLineParser.getFlagValue("server");
		if (server == null) {
			server = "https://localhost:8443";
			System.err.println("Missing argument: server - using default");
		}
		// TODO: get env id legitimately
		String agent = commandLineParser.getFlagValue("agent");
		if (agent == null) {
			System.err.println("Missing required argument: agent");
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
		JSONObject resourceJSONObject = resourceClient.getResourceRoleByName(agent);
		if (debug) {
			System.out.println(agent + ":\n" + resourceJSONObject);
		}
		for (String field : fields) {
			try {
				if (resourceJSONObject.getString(field) != null
						&& resourceJSONObject.getString(field) != "") {
					System.out.println(String.format("%s: \"%s\"", field,
							resourceJSONObject.getString(field)));
				}
			} catch (JSONException e) {
				// ignore
			}
		}
	}

}
