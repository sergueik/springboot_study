package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.File;
import java.math.BigDecimal;
import java.nio.file.FileSystemException;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import se.jhaals.Vault;
import se.jhaals.VaultResponse;

public class Application {

	private static boolean debug = false;
	private static Vault vault;

	@SuppressWarnings("deprecation")
	public static void main(String[] args) throws Exception {

		Map<String, String> cli = parseArgs(args);

		String token = "c7e6d2f3-dc1a-a841-cf29-0cf7bec8ed42"; // System.getenv("VAULT_TOKEN");

		String server = "localhost";
		String scheme = "http";
		int port = 8200;
		String dir = "sftp";

		if (cli.containsKey("debug")) {
			debug = true;
		}

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("token")) {
			System.err.println(String.format("Usage: jar "
					+ "-token <token> -server <server> -port <port> -dir <dir>\r\n"));
			return;
		}
		if (cli.containsKey("server"))
			server = cli.get("server");
		if (cli.containsKey("token"))
			token = cli.get("token");
		if (cli.containsKey("port"))
			port = Integer.parseInt(cli.get("port"));

		if (cli.containsKey("dir"))
			dir = cli.get("dir");

		try {
			String vault_server_url = scheme + "://" + server + ":" + port;
			vault = new Vault(vault_server_url, token);

			VaultResponse vaultResponse = vault.read(dir);
			System.out.println(vaultResponse.getData().get("public_key"));
			System.out.println(vaultResponse.getData().get("srivate_key"));

		} catch (Exception e) {
			System.out.println(e.toString() + " " + e.getMessage());
			e.printStackTrace();
		}
	}

	// Extremely simple CLI parser: -key value
	private static Map<String, String> parseArgs(String[] args) {
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
