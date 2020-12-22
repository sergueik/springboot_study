package example;

/**
 * Copyright 2020 Serguei Kouzmine
 */
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.urbancode.ud.client.AgentClient;
import com.urbancode.ud.client.ComponentClient;
import com.urbancode.ud.client.ResourceClient;

/*
 * This example exercises browsing versioned properties (unfinished)
 * */
public class GetResourceRoleProperties extends Common {
	private static final List<String> fields = Arrays.asList("id", "name",
			"description");

	private static ResourceClient resourceClient;
	private static ComponentClient componentClient;
	private static AgentClient agentClient;
	private static JSONObject data;
	private static String env;
	private static String agent;
	private static int index;
	private static final List<JSONObject> defs = new ArrayList<>();
	private static final List<JSONObject> values = new ArrayList<>();

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {
		configure(args);
		commandLineParser.saveFlagValue("env");
		commandLineParser.saveFlagValue("agent");
		commandLineParser.parse(args);

		env = commandLineParser.getFlagValue("env");
		if (env == null) {
			System.err.println("Missing required argument: env");
			return;
		}
		agent = commandLineParser.getFlagValue("agent");
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
		// TODO: switch to agentClient.
		// NOTE: following additional API available
		// getResourceChildren
		// getResourceRoles
		// getResourceRolePropertyForResource
		// getResourceRolesAsStrings
		// setResourceRoleProperty
		JSONObject roleJSONObject = resourceClient.getResourceRoleByName(agent);
		String id3 = "172f1d3c-3665-6fd2-1c71-f0e67ec633c5";
		System.out
				.println("Calling for " + id3 + " " + roleJSONObject.getString("id"));
		JSONArray propertiesForRoleJSONArray = resourceClient
				.getResourceRoleProperties(id3, roleJSONObject.getString("id"));
		for (index = 0; index < propertiesForRoleJSONArray.length(); index++) {
			JSONObject propertiesForRoleJSONObject = propertiesForRoleJSONArray
					.getJSONObject(index);
			//
			JSONObject propDefJSONObject = propertiesForRoleJSONObject
					.getJSONObject("propDef");
			JSONObject propValueJSONObject = propertiesForRoleJSONObject
					.getJSONObject("propValue");
			defs.add(propDefJSONObject);
			values.add(propValueJSONObject);

			// System.out.println(prettyPrint(propDefJSONObject));
			// System.out.println(prettyPrint(propValueJSONObject));
		}

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
