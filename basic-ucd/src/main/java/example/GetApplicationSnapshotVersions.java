package example;

import java.io.BufferedReader;
/**
 * Copyright 2020 Serguei Kouzmine
 */
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.Arrays;
import java.util.List;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.urbancode.ud.client.ApplicationClient;
import com.urbancode.ud.client.ComponentClient;

/*
 * This example exercises
 * ApplicationClient.getSnapshotVersions
 * file:///home/sergueik/src/springboot_study/basic-ucd/uDeployRestClient/docs/index.html
 * */
public class GetApplicationSnapshotVersions extends Common {
	private static final List<String> componentFields = Arrays.asList("id",
			"name", "description");
	// the "desiredVersions" processed separately
	private static final List<String> versionFields = Arrays.asList("id", "name",
			"created", "description");

	private static ApplicationClient applicationClient;
	private static JSONObject componentData;
	private static JSONArray componentDataArray;
	private static JSONObject versionData;
	private static JSONArray versionDataArray;
	private static int index;
	private static int index2;

	private static String data;
	private static String application;
	private static String snapshot;

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {

		configure(args);
		commandLineParser.saveFlagValue("data");
		commandLineParser.saveFlagValue("application");
		commandLineParser.saveFlagValue("snapshot");
		commandLineParser.parse(args);

		data = commandLineParser.getFlagValue("data");
		if (data == null) {
			application = commandLineParser.getFlagValue("application");
			if (application == null) {
				System.err.println("Missing required argument: application");
				return;
			}
			snapshot = commandLineParser.getFlagValue("snapshot");
			if (snapshot == null) {
				System.err.println("Missing required argument: snapshot");
				return;
			}
			// explore resource hierarchy
			applicationClient = new ApplicationClient(new URI(server), user,
					password);

			if (applicationClient == null) {
				throw new RuntimeException(String.format(
						"failed to connect to server %s as user: %s / password: %s", server,
						user, password));
			}
			componentDataArray = applicationClient.getSnapshotVersions(snapshot);
		} else {
			componentDataArray = readJSONArray(data);
		}

		for (index = 0; index != componentDataArray.length(); index++) {
			componentData = componentDataArray.getJSONObject(index);

			System.out.println("  - ");
			if (debug) {
				System.out.println(snapshot + ":\n" + prettyPrint(componentData));
			}
			// TODO: process
			versionDataArray = componentData.getJSONArray("desiredVersions");
			if (versionDataArray.length() == 0) {
				continue;
			}
			for (index2 = 0; index2 != versionDataArray.length(); index2 ++) {
				versionData = versionDataArray.getJSONObject(index2);
				for (String field : versionFields) {
					try {
						if (versionData.getString(field) != null
								&& versionData.getString(field) != "") {
							System.out.println(String.format("  %s: \"%s\"", field,
									versionData.getString(field)));
						}
					} catch (JSONException e) {
						// ignore
					}
				}
			}
			for (String field : componentFields) {
				try {
					if (componentData.getString(field) != null
							&& componentData.getString(field) != "") {
						System.out.println(String.format("  %s: \"%s\"", field,
								componentData.getString(field)));
					}
				} catch (JSONException e) {
					// ignore
				}
			}
		}
	}

	private static JSONArray jsonArray = null;
	private static String text;
	private static boolean debug = false;

	public static String readRawJSON(String url) throws IOException {
		InputStream is = new URL(url).openStream();
		try {
			BufferedReader rd = new BufferedReader(
					new InputStreamReader(is, Charset.forName("UTF-8")));
			text = readAll(rd);
			if (debug)
				System.err.println("Read JSON data: " + text);
			return text;
		} finally {
			is.close();
		}
	}

	public static JSONArray readJSONArray(String url)
			throws IOException, JSONException {
		text = readRawJSON(url);
		if (debug)
			System.err.println("Read JSON data: " + text);
		jsonArray = new JSONArray(text);
		return jsonArray;
	}

	private static String readAll(Reader rd) throws IOException {
		StringBuilder sb = new StringBuilder();
		int cp;
		while ((cp = rd.read()) != -1) {
			sb.append((char) cp);
		}
		return sb.toString();
	}
}
