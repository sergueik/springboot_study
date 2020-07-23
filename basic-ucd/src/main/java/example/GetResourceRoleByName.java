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

import com.urbancode.ud.client.ResourceClient;

/*
 * This example exercises
 * ResourceClient.getResourceByPath
 * */
public class GetResourceRoleByName extends Common {
	private static final List<String> fields = Arrays.asList("id", "name", "path",
			"description");

	private static ResourceClient resourceClient;
	private static JSONObject data;

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {

		configure(args);

		commandLineParser.saveFlagValue("path");
		commandLineParser.parse(args);

		// TODO: get env id legitimately
		String path = commandLineParser.getFlagValue("path");
		if (path == null) {
			System.err.println("Missing required argument: path");
			return;
		}
		// explore resource hierarchy
		resourceClient = new ResourceClient(new URI(server), user, password);

		if (resourceClient == null) {
			throw new RuntimeException(String.format(
					"failed to connect to server %s as user: %s / password: %s", server,
					user, password));
		}
		data = resourceClient.getResourceByPath(path);
		if (debug) {
			System.out.println(path + ":\n" + data);
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
