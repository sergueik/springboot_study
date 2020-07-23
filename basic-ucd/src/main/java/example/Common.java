package example;

/**
 * Copyright 2020 Serguei Kouzmine
 */
import java.io.IOException;
import java.net.URISyntaxException;

import org.codehaus.jettison.json.JSONException;

public class Common {

	protected static boolean debug = false;
	protected static boolean verbose = false;

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
			server = "https://localhost:8443";
			System.err.println("Missing argument: server - using default");
		}
	}
}
