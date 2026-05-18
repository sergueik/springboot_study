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

public class SFTPKeyClientUpload {

    private static boolean debug = false;

    public static void main(String[] args) {

        try {

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

            if (debug) {
                System.err.println("args: " + cli.keySet());
            }

            if (cli.containsKey("help") || !cli.containsKey("filepath")) {
                System.err.println("Usage: -filepath <file> -server <host> -port <port> -user <user> -keydir <keydir>");
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

            FileSystemManager manager = VFS.getManager();

            if (debug) {
                System.out.println("schemes: " +
                        Arrays.toString(manager.getSchemes()));

                System.out.println("CTX CL = " +
                        Thread.currentThread().getContextClassLoader());

                System.out.println("VFS CL = " +
                        VFS.class.getClassLoader());
            }

            File file = new File(filepath);
            if (!file.exists()) {
                throw new RuntimeException("Local file not found: " + filepath);
            }

            FileSystemOptions opts = new FileSystemOptions();

            SftpFileSystemConfigBuilder cfg =
                    SftpFileSystemConfigBuilder.getInstance();

            cfg.setStrictHostKeyChecking(opts, "no");
            cfg.setUserDirIsRoot(opts, true);
            cfg.setConnectTimeout(opts, java.time.Duration.ofSeconds(10));
            cfg.setSessionTimeout(opts, java.time.Duration.ofSeconds(10));

            String sshDir = System.getenv().getOrDefault(
                    "SFTP_SSH_DIR",
                    System.getProperty("user.home") + "/" + keydir
            );

            IdentityInfo identities = new IdentityInfo(
                    new File(sshDir, "sftpuser_key"),
                    new File(sshDir, "sftpuser_key.pub"),
                    null
            );

            cfg.setIdentityInfo(opts, identities);

            String sftpUri = String.format(
                    "sftp://%s@%s:%d%s/%s",
                    user, server, port, targetdir, filepath
            );

            if (debug) {
                System.out.println("SFTP URI: " + sftpUri);
            }

            FileObject localFile = manager.resolveFile(file.getAbsolutePath());
            FileObject remoteFile = manager.resolveFile(sftpUri, opts);

            remoteFile.copyFrom(localFile, Selectors.SELECT_SELF);

            System.out.println("File upload successful");

        } catch (Exception e) {
            System.out.println(e.toString());
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
