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
import se.jhaals.VaultException;
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
		String dir = null;
		String key = null;

		if (cli.containsKey("debug")) {
			debug = true;
		}

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("token") || !cli.containsKey("dir")
				|| !cli.containsKey("key")) {
			System.err.println(String
					.format("Usage: jar " + "-token <token> -server <server> -port <port> -dir <dir> -key <key>\r\n"));
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
		if (cli.containsKey("key"))
			key = cli.get("key");

		try {
			String vault_server_url = scheme + "://" + server + ":" + port;
			vault = new Vault(vault_server_url, token);

			VaultResponse vaultResponse = vault.read(dir);
			@SuppressWarnings("unchecked")
			Map<String, Object> data = (Map<String, Object>) vaultResponse.getData().get("data");
			System.out.println(String.format("key: %s value: %s", key, data.get(key)));
		} catch (VaultException e) {
			System.out.println("Vault Exception: " + String.join("\n", e.getMessages()));
		} catch (Exception e) {
			System.out.println(e.toString() + " " + e.getMessage());
			e.printStackTrace();
		}
	}

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
