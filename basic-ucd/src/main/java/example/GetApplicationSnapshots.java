package example;

import java.io.BufferedReader;
/**
 * Copyright 2020,2021 Serguei Kouzmine
 */
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.urbancode.ud.client.ApplicationClient;

/*
 * This example exercises
 * ApplicationClient.getSnapshot
 * 
 * and own "extension" method CustomApplicationClient.getSnapshots
 * file:///home/sergueik/src/springboot_study/basic-ucd/uDeployRestClient/docs/index.html
 * */
public class GetApplicationSnapshots extends Common {
	private static JSONArray jsonArray = null;
	private static String text;
	private static boolean debug = false;

	private static final List<String> componentFields = Arrays.asList("id", "name", "description");
	// the "desiredVersions" processed separately
	private static final List<String> versionFields = Arrays.asList("id", "name", "created", "description");

	// NOTE: similar situation with /cli/resource/getPrroperties "REST" API
	// missing from
	// com.urbancode.ud.client.ResourceClient class
	private static CustomApplicationClient applicationClient;
	private static JSONObject componentData;
	private static JSONArray componentDataArray;
	private static boolean status = false;
	private static int index;

	private static String data;
	private static String application;
	private static String snapshot;
	private static final List<String> names = new ArrayList<>();
	private static final List<String> ids = new ArrayList<>();

	public static void main(String[] args) throws URISyntaxException, IOException, JSONException {

		configure(args);
		commandLineParser.saveFlagValue("data");
		commandLineParser.saveFlagValue("application");
		commandLineParser.saveFlagValue("snapshot");
		commandLineParser.saveFlagValue("op");
		commandLineParser.parse(args);

		String op = commandLineParser.getFlagValue("op");
		if (op == null) {
			System.err.println("Missing required argument: op");
			return;
		}
		data = commandLineParser.getFlagValue("data");
		if (data == null) {
			application = commandLineParser.getFlagValue("application");
			if (application == null) {
				System.err.println("Missing required argument: application");
				return;
			}
			// explore resource hierarchy
			applicationClient = new CustomApplicationClient(new URI(server), user, password);

			if (applicationClient == null) {
				throw new RuntimeException(String.format("failed to connect to server %s as user: %s / password: %s",
						server, user, password));
			}
			if (op.equalsIgnoreCase("list")) {
				componentDataArray = applicationClient.getSnapshots(application);
				for (index = 0; index < componentDataArray.length(); index++) {
					names.add(componentDataArray.getJSONObject(index).getString("name"));
					ids.add((String) componentDataArray.getJSONObject(index).get("id"));
				}
				System.err.println(String.join("\n", names));
			}
			if (op.equalsIgnoreCase("confirm")) {

				snapshot = commandLineParser.getFlagValue("snapshot");
				if (snapshot == null) {
					System.err.println("Missing required argument: snapshot");
					return;
				}
				try {
					// ignore the result details
					applicationClient.getSnapshot(application, snapshot);
					status = true;
					System.err.println(String.format("Snapshot \"%s\" confirmed to be valid", snapshot));
				} catch (IOException e) {
					System.err.println("Exception (collected): " + e.getMessage());
					status = false;
				}
				System.err.println("Status: " + status);
			}
		} else {
			componentDataArray = readJSONArray(data);
			for (index = 0; index < componentDataArray.length(); index++) {
				names.add(componentDataArray.getJSONObject(index).getString("name"));
				ids.add((String) componentDataArray.getJSONObject(index).get("id"));
			}
			if (op.equalsIgnoreCase("list")) {
				System.err.println(String.join("\n", names));
			}
			if (op.equalsIgnoreCase("confirm")) {
				snapshot = commandLineParser.getFlagValue("snapshot");
				if (snapshot == null) {
					System.err.println("Missing required argument: snapshot");
					return;
				}
				if (names.contains(snapshot)) {
					status = true;
					System.err.println(String.format("Snapshot \"%s\" confirmed to be valid", snapshot));
				} else {
					System.err.println(String.format("Snapshot \"%s\" is not valid", snapshot));
					status = false;
				}
				System.err.println("Status: " + status);
			}
		}
	}

	// TODO: move to Common
	public static String readRawJSON(String url) throws IOException {
		InputStream is = new URL(url).openStream();
		try {
			BufferedReader rd = new BufferedReader(new InputStreamReader(is, Charset.forName("UTF-8")));
			text = readAll(rd);
			if (debug)
				System.err.println("Read JSON data: " + text);
			return text;
		} finally {
			is.close();
		}
	}

	// TODO: move to Common
	public static JSONArray readJSONArray(String url) throws IOException, JSONException {
		text = readRawJSON(url);
		if (debug)
			System.err.println("Read JSON data: " + text);
		jsonArray = new JSONArray(text);
		return jsonArray;
	}

	// TODO: move to Common
	private static String readAll(Reader rd) throws IOException {
		StringBuilder sb = new StringBuilder();
		int cp;
		while ((cp = rd.read()) != -1) {
			sb.append((char) cp);
		}
		return sb.toString();
	}

	public static class CustomApplicationClient extends ApplicationClient {
		public CustomApplicationClient(URI url, String clientUser, String clientPassword) {
			super(url, clientUser, clientPassword);
		}

		public JSONArray getSnapshots(String application) throws IOException, JSONException {

			// Final path parameter is true/false for inactive snapshots
			// String uri = url + "/rest/deploy/application/" +
			// encodePath(application)
			// + "/snapshots/false";
			// HttpGet method = new HttpGet(uri);

			// NOTE: invokeMethod is protected in UDRestClient
			// HttpResponse response = invokeMethod(method);
			// String body = getBody((CloseableHttpResponse) invokeMethod(method));
			// JSONArray resultJSON = new JSONArray(body);
			// return resultJSON;
			return new JSONArray(getBody((CloseableHttpResponse) invokeMethod(
					new HttpGet(url + "/rest/deploy/application/" + encodePath(application) + "/snapshots/false"))));
		}

	}
}
