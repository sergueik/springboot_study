package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemManager;
import org.apache.commons.vfs2.FileSystemOptions;
import org.apache.commons.vfs2.Selectors;
import org.apache.commons.vfs2.VFS;
import org.apache.commons.vfs2.provider.sftp.IdentityInfo;
import org.apache.commons.vfs2.provider.sftp.SftpFileSystemConfigBuilder;

import java.io.File;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class SFTPClient {

	public static boolean debug = false;

	public static void downloadFile(String filepath, String server, int port, String user, String targetdir,
			String keydir) {
		try {

			FileSystemManager manager = VFS.getManager();

			if (debug) {
				System.out.println("schemes: " + Arrays.toString(manager.getSchemes()));

				System.out.println("CTX CL = " + Thread.currentThread().getContextClassLoader());

				System.out.println("VFS CL = " + VFS.class.getClassLoader());
			}


			FileSystemOptions opts = new FileSystemOptions();

			SftpFileSystemConfigBuilder cfg = SftpFileSystemConfigBuilder.getInstance();

			cfg.setStrictHostKeyChecking(opts, "no");
			cfg.setUserDirIsRoot(opts, true);
			cfg.setConnectTimeout(opts, java.time.Duration.ofSeconds(10));
			cfg.setSessionTimeout(opts, java.time.Duration.ofSeconds(10));

			String sshDir = System.getenv().getOrDefault("SFTP_SSH_DIR",
					System.getProperty("user.home") + "/" + keydir);

			IdentityInfo identities = new IdentityInfo(new File(sshDir, "sftpuser_key"),
					new File(sshDir, "sftpuser_key.pub"), null);

			cfg.setIdentityInfo(opts, identities);

			String uri = String.format("sftp://%s@%s:%d%s/%s", user, server, port, targetdir, filepath);

			if (debug) {
				System.out.println("SFTP URI: " + uri);
			}

			File file = new File(filepath);
			FileObject localFile = manager.resolveFile(file.getAbsolutePath());
			FileObject remoteFile = manager.resolveFile(uri, opts);
			localFile.copyFrom(remoteFile, Selectors.SELECT_SELF);
			// check if the file exists
			if (!file.exists()) {
				throw new RuntimeException("Error. Local file not found");
			}

		} catch (Exception e) {
			System.out.println(e.toString());
			e.printStackTrace();
		}
	}

	public static void uploadFile(String filepath, String server, int port, String user, String targetdir,
			String keydir) {

		try {

			FileSystemManager manager = VFS.getManager();
			/*
			if (debug) {
				System.out.println("schemes: " + Arrays.toString(manager.getSchemes()));
				System.out.println("CTX CL = " + Thread.currentThread().getContextClassLoader());
				System.out.println("VFS CL = " + VFS.class.getClassLoader());
			}
			 */
			File file = new File(filepath);
			if (!file.exists()) {
				throw new RuntimeException("Local file not found: " + filepath);
			}

			FileSystemOptions opts = new FileSystemOptions();

			SftpFileSystemConfigBuilder cfg = SftpFileSystemConfigBuilder.getInstance();

			cfg.setStrictHostKeyChecking(opts, "no");
			cfg.setUserDirIsRoot(opts, true);
			cfg.setConnectTimeout(opts, java.time.Duration.ofSeconds(10));
			cfg.setSessionTimeout(opts, java.time.Duration.ofSeconds(10));

			String sshDir = System.getenv().getOrDefault("SFTP_SSH_DIR",
					System.getProperty("user.home") + "/" + keydir);

			IdentityInfo identities = new IdentityInfo(new File(sshDir, "sftpuser_key"),
					new File(sshDir, "sftpuser_key.pub"), null);

			cfg.setIdentityInfo(opts, identities);

			String uri = String.format("sftp://%s@%s:%d%s/%s", user, server, port, targetdir, filepath);

			if (debug) {
				System.out.println("URI: " + uri);
			}

			FileObject localFile = manager.resolveFile(file.getAbsolutePath());
			FileObject remoteFile = manager.resolveFile(uri, opts);

			remoteFile.copyFrom(localFile, Selectors.SELECT_SELF);
		} catch (Exception e) {
			System.out.println(e.toString());
			e.printStackTrace();
		}
	}

}

