package example;

/**
 * Copyright 2020 Serguei Kouzmine
 */
import java.io.IOException;
import java.net.URISyntaxException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

public class Common {

	protected static boolean debug = false;
	protected static boolean verbose = false;
	private static final int spacesToIndentEachLevel = 2;

	protected static String user;
	protected static String password;
	protected static String server;

	protected static CommandLineParser commandLineParser;

	public static void configure(String[] args)
			throws URISyntaxException, IOException, JSONException {
		commandLineParser = new CommandLineParser();
		commandLineParser.saveFlagValue("user");
		commandLineParser.saveFlagValue("password");
		commandLineParser.saveFlagValue("server");

		// can one repeatedly parse ?
		commandLineParser.parse(args);

		if (commandLineParser.hasFlag("debug")) {
			debug = true;
		}
		if (commandLineParser.hasFlag("verbose")) {
			verbose = true;
		}

		user = commandLineParser.getFlagValue("user");
		if (user == null) {
			user = "admin";
			System.err.println("Missing argument: user - using default");
		}
		password = commandLineParser.getFlagValue("password");
		if (password == null) {
			password = "admin";
			System.err.println("Missing argument: password - using default");
		}
		server = commandLineParser.getFlagValue("server");

		if (server == null) {
			server = String.format("https://%s:8443",
					getEnv("UCD_SERVER_IP", "localhost"));
			System.err.println("Missing argument: server - using default " + server);
		}
	}

	public static String getEnv(String name, String defaultValue) {
		String value = null;
		if (debug) {
			System.err.println("Trying environment " + name);
		}
		value = System.getenv(name);
		if (value == null) {
			if (debug) {
				System.err.println("Nothing found for " + name);
			}
			value = defaultValue;
		}
		return value;
	}

	public String getPropertyEnv(String name, String defaultValue) {
		String value = System.getProperty(name);
		if (debug) {
			System.err.println("Getting propety or environment: " + name);
		}
		// compatible with
		// org.apache.commons.configuration.PropertiesConfiguration.interpolatedConfiguration
		// https://commons.apache.org/proper/commons-configuration/userguide_v1.10/howto_utilities.html
		if (value == null) {

			Pattern p = Pattern.compile("^(\\w+:)(\\w+)$");
			Matcher m = p.matcher(name);
			if (m.find()) {
				String propertyName = m.replaceFirst("$2");
				if (debug) {
					System.err.println("Interpolating " + propertyName);
				}
				value = System.getProperty(propertyName);
			}
			if (value == null) {
				if (debug) {
					System.err.println("Trying environment " + name);
				}
				value = System.getenv(name);
				if (value == null) {
					if (debug) {
						System.err.println("Nothing found for " + name);
					}
					value = defaultValue;
				}
			}
		}
		return value;
	}

	// based on:
	// https://stackoverflow.com/questions/6185337/how-do-i-pretty-print-existing-json-data-with-java
	protected static String prettyPrint(JSONObject data) {
		try {
			return new JSONObject(data.toString()).toString(spacesToIndentEachLevel);
		} catch (JSONException e) {
			return null;
		}
	}

	protected static String prettyPrint(String data) {
		try {
			return new JSONObject(data).toString(spacesToIndentEachLevel);
		} catch (JSONException e) {
			return null;
		}
	}
}
