package example;

/**
 * Copyright 2020 Serguei Kouzmine
 */
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.Arrays;
import java.util.List;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.urbancode.ud.client.ComponentClient;

/*
 * This example exercises
 * ResourceClient.getResourceByPath
 * ResourceClient.getResourceById
 * file:///home/sergueik/src/springboot_study/basic-ucd/uDeployRestClient/docs/index.html
 * */
public class GetComponent extends Common {
	private static final List<String> fields = Arrays.asList("id", "name", "path",
			"description");

	private static ComponentClient componentClient;
	private static JSONObject data;
	private static String component;

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {

		configure(args);
		commandLineParser.saveFlagValue("component");
		commandLineParser.parse(args);

		component = commandLineParser.getFlagValue("component");
		if (component == null) {
			System.err.println("Missing required argument: component");
			return;
		}
		// explore resource hierarchy
		componentClient = new ComponentClient(new URI(server), user, password);

		if (componentClient == null) {
			throw new RuntimeException(String.format(
					"failed to connect to server %s as user: %s / password: %s", server,
					user, password));
		}
		data = componentClient.getComponent(component);

		if (debug) {
			System.out.println(component + ":\n" + prettyPrint(data));
		}
		for (String field : fields) {
			try {
				if (data.getString(field) != null && data.getString(field) != "") {
					System.out.println(
							String.format("%s: \"%s\"", field, data.getString(field)));
				}
			} catch (JSONException e) {
				// ignore
			}
		}
	}

}
