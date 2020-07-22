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
 * */
public class GetResourceChildren {
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
		commandLineParser.saveFlagValue("newname");
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
		String newname = commandLineParser.getFlagValue("newname");
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
						System.out.println("Object: " + resourceChildObject);

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
}
