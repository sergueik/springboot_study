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
 * ResourceClient.getResourceById
 * file:///home/sergueik/src/springboot_study/basic-ucd/uDeployRestClient/docs/index.html
 * */
public class GetResourceByIdOrPath extends Common {
	private static final List<String> fields = Arrays.asList("id", "name", "path", "description");

	private static ResourceClient resourceClient;
	private static JSONObject data;
	private static String path;
	private static String id;

	public static void main(String[] args) throws URISyntaxException, IOException, JSONException {

		configure(args);
		// TODO: vararg
		commandLineParser.saveFlagValue("path");
		commandLineParser.saveFlagValue("id");
		commandLineParser.parse(args);

		// TODO: get resource id legitimately
		path = commandLineParser.getFlagValue("path");
		id = commandLineParser.getFlagValue("id");
		if ((id == null) && (path == null)) {
			System.err.println("Missing required argument: path or id");
			return;
		}
		// explore resource hierarchy
		resourceClient = new ResourceClient(new URI(server), user, password);

		if (resourceClient == null) {
			throw new RuntimeException(
					String.format("failed to connect to server %s as user: %s / password: %s", server, user, password));
		}
		data = (id != null) ? resourceClient.getResourceById(id) :
				resourceClient.getResourceByPath(path);

		if (debug) {
			System.out.println(path + ":\n" + data);
		}
		for (String field : fields) {
			try {
				if (data.getString(field) != null && data.getString(field) != "") {
					System.out.println(String.format("%s: \"%s\"", field, data.getString(field)));
				}
			} catch (JSONException e) {
				// ignore
			}
		}
	}

}
