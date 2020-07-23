package example;

/**
 * Copyright 2020 Serguei Kouzmine
 */
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.List;
import java.util.UUID;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.urbancode.ud.client.AgentClient;
import com.urbancode.ud.client.ComponentClient;
import com.urbancode.ud.client.ResourceClient;

/*
 * */
public class GetResourceChildren extends Common {
	private static final List<String> fields = Arrays.asList("id", "name",
			"description");
	private static final List<String> fields2 = Arrays.asList("name",
			"description", "value");
	private static final List<String> fields3 = Arrays.asList("name", "path",
			"description");

	private static String env;

	private static ResourceClient resourceClient;
	private static ComponentClient componentClient;
	private static AgentClient agentClient;
	private static JSONObject data;

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {

		configure(args);

		// can repeatedly parse
		commandLineParser.parse(args);
		commandLineParser.saveFlagValue("env");

		commandLineParser.parse(args);

		// TODO: get env id legitimately
		env = commandLineParser.getFlagValue("env");
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
