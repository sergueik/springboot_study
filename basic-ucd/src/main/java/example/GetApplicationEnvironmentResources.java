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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.http.client.methods.CloseableHttpResponse;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpRequestBase;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.urbancode.ud.client.ApplicationClient;
import com.urbancode.ud.client.ComponentClient;
import com.urbancode.ud.client.ResourceClient;

/*
 * This example exercises
 * ApplicationClient.getSnapshot
 * 
 * and own "extension" method CustomApplicationClient.getSnapshots
 * file:///home/sergueik/src/springboot_study/basic-ucd/uDeployRestClient/docs/index.html
 * */
public class GetApplicationEnvironmentResources extends Common {
	private static JSONArray jsonArray = null;
	private static String text;
	private static boolean debug = false;

	private static final List<String> fields = Arrays.asList("id", "name",
			"description");

	// NOTE: similar situation with /cli/resource/getPrroperties "REST" API
	// missing from
	// com.urbancode.ud.client.ResourceClient class
	private static CustomApplicationClient applicationClient;
	private static JSONArray agentDataArray;
	private static int index;
	private String id;
	private static String data;
	private static String application;
	private static final List<String> names = new ArrayList<>();
	private static final List<String> ids = new ArrayList<>();

	public static void main(String[] args)
			throws URISyntaxException, IOException, JSONException {

		configure(args);
		commandLineParser.saveFlagValue("application");
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
			applicationClient = new CustomApplicationClient(new URI(server), user,
					password);

			if (applicationClient == null) {
				throw new RuntimeException(String.format(
						"failed to connect to server %s as user: %s / password: %s", server,
						user, password));
			}
			if (op.equalsIgnoreCase("list")) {
				agentDataArray = applicationClient
						.getApplicationEnvironments(application, "true", "false");
				for (index = 0; index < agentDataArray.length(); index++) {
					names.add(agentDataArray.getJSONObject(index).getString("name"));
					ids.add((String) agentDataArray.getJSONObject(index).get("id"));
				}
				System.err.println(String.join("\n", names));
				for (String id : ids) {
					JSONArray environmentResourceJsonArray = applicationClient
							.getEnvironmentResources(id);

					for (int index2 = 0; index2 != environmentResourceJsonArray
							.length(); index2++) {
						JSONObject environmentResourceObject = environmentResourceJsonArray
								.getJSONObject(index2);
						for (String field : fields) {
							if (environmentResourceObject.getString(field) != null
									&& environmentResourceObject.getString(field) != "") {
								System.out.println(String.format("      %s: \"%s\"", field,
										environmentResourceObject.getString(field)));
							}
						}

					}
				}
			}
		}

		/*
				for (int i = 0; i < resultJSON.length(); i++) {
						JSONObject snapshotJson = (JSONObject) resultJSON.get(i);
						String resultName = (String) snapshotJson.get("name");
						String resultId = (String) snapshotJson.get("id");
						if (snapshot.equals(resultName) || snapshot.equals(resultId)) {
							result = snapshotJson;
							break;
						}
					}
		
		 * */
	}

	// TODO: move to Common
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

	// TODO: move to Common
	public static JSONArray readJSONArray(String url)
			throws IOException, JSONException {
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
		public CustomApplicationClient(URI url, String clientUser,
				String clientPassword) {
			super(url, clientUser, clientPassword);
		}

		public JSONArray getEnvironmentResources(String environmentId)
				throws IOException, JSONException {
			JSONArray result = null;
			String uri = url + "/rest/deploy/environment/" + encodePath(environmentId)
					+ "/resources";

			HttpGet method = new HttpGet(uri);
			CloseableHttpResponse response = invokeMethod(method);
			result = new JSONArray(getBody(response));
			return result;
		}
	}
}
