package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileSystemOptions;
import org.apache.commons.vfs2.Selectors;
import org.apache.commons.vfs2.impl.StandardFileSystemManager;
import org.apache.commons.vfs2.provider.sftp.IdentityInfo;
import org.apache.commons.vfs2.provider.sftp.SftpFileSystemConfigBuilder;

import java.io.File;
import java.math.BigDecimal;
import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

public class SFTPKeyClientDownload {

	private static boolean debug = false;

	@SuppressWarnings("deprecation")
	public static void main(String[] args) throws Exception {

		Map<String, String> cli = parseArgs(args);
		String server = "localhost";
		String targetdir = "/data";
		String keydir = ".ssh_keys/simple-sftp";
		int port = 2222;
		String user = "sftpuser";
		String filepath = "SFTP_UPLOADED_WITH_KEY.txt";

		if (cli.containsKey("debug")) {
			debug = true;
		}

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("filepath")) {
			System.err.println(String.format("Usage: jar "
					+ "-filepath <filepath> -server <server> -port <port> -user <user> -keydir <keydir>\r\n"));
			return;
		}
		if (cli.containsKey("server"))
			server = cli.get("server");
		if (cli.containsKey("filepath"))
			filepath = cli.get("filepath");
		if (cli.containsKey("port"))
			port = Integer.parseInt(cli.get("port"));

		if (cli.containsKey("user"))
			user = cli.get("user");
		if (cli.containsKey("keydir"))
			keydir = cli.get("keydir");

		try {

			StandardFileSystemManager manager = new StandardFileSystemManager();

			// check if the file exists
			File file = new File(filepath);
			if (!file.exists()) {
				throw new RuntimeException("Error. Local file not found");
			}

			// Initializes the file manager
			manager.init();

			// Setup our SFTP configuration
			FileSystemOptions opts = new FileSystemOptions();
			SftpFileSystemConfigBuilder.getInstance().setStrictHostKeyChecking(opts, "no");
			SftpFileSystemConfigBuilder.getInstance().setUserDirIsRoot(opts, true);
			SftpFileSystemConfigBuilder.getInstance().setConnectTimeout(opts, java.time.Duration.ofSeconds(10));
			SftpFileSystemConfigBuilder.getInstance().setSessionTimeout(opts, java.time.Duration.ofSeconds(10));
			String sshDir = System.getenv().getOrDefault("SFTP_SSH_DIR",
					System.getProperty("user.home") + "/" + keydir);
			IdentityInfo identities = new IdentityInfo(new File(sshDir + "/sftpuser_key"), // Private key
					new File(sshDir + "/sftpuser_key.pub"), // Public key
					null // Passphrase
			);

			SftpFileSystemConfigBuilder.getInstance().setIdentityInfo(opts, identities);

			// Create the SFTP URI using the host name, userid, no password, remote path and
			// file name

			String sftpUri = "sftp://" + user + ":@" + server + ":" + port + targetdir + "/" + filepath;
			// Create local file object
			FileObject localFile = manager.resolveFile(file.getAbsolutePath());

			// Create remote file object
			FileObject remoteFile = manager.resolveFile(sftpUri, opts);

			// Copy local file from sftp server
			localFile.copyFrom(remoteFile, Selectors.SELECT_SELF);
			System.out.println("File download successfully");

		} catch (FileSystemException e) {
			System.out.println(e.toString());
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
