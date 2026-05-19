package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.File;
import java.math.BigDecimal;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

@SuppressWarnings("unused")
public class Runner {

	private static boolean debug = false;

	@SuppressWarnings("deprecation")
	public static void main(String[] args) throws Exception {

		Map<String, String> cli = parseArgs(args);

		String server = "localhost";
		String targetdir = "/data";
		String keydir = ".ssh_keys/simple-sftp";
		int port = 2222;
		String user = "sftpuser";
		String filepath = null;
		String operation = null;

		if (cli.containsKey("debug")) {
			debug = true;
		}

		if (debug) {
			System.err.println("args: " + cli.keySet());
		}

		if (cli.containsKey("help") || !cli.containsKey("filepath")) {
			System.err.println(
					"Usage: -filepath <file> -server <host> -port <port> -user <user> -keydir <keydir> [-debug true]");
			return;
		}

		if (cli.containsKey("server")) {
			server = cli.get("server");
		}
		if (cli.containsKey("filepath")) {
			filepath = cli.get("filepath");
		}
		if (cli.containsKey("port")) {
			port = Integer.parseInt(cli.get("port"));
		}
		if (cli.containsKey("user")) {
			user = cli.get("user");
		}
		if (cli.containsKey("keydir")) {
			keydir = cli.get("keydir");
		}

		if (cli.containsKey("operation"))
			operation = cli.get("operation");

		if (operation == null) {
			System.err.println("Missing required argument: operation");
			return;
		}
		SFTPClient.debug = debug;
		if (operation.equalsIgnoreCase("upload")) {
			SFTPClient.uploadFile(filepath, server, port, user, targetdir, keydir);
		}

		if (operation.equalsIgnoreCase("download")) {
			SFTPClient.downloadFile(filepath, server, port, user, targetdir, keydir);
		}
		System.err.println("Done: " + operation);
	}

	private static Map<String, String> parseArgs(String[] args) {
		if (Arrays.asList(args).contains("debug"))
			System.err.println("Processing: " + Arrays.asList(args));
		Map<String, String> map = new HashMap<>();
		for (int i = 0; i < args.length - 1; i++) {
			if (args[i].startsWith("-")) {
				map.put(args[i].substring(1), args[i + 1]);
				i++;
			}
		}
		return map;
	}
}
