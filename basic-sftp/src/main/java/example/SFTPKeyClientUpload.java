package example;

import java.io.File;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileSystemOptions;
import org.apache.commons.vfs2.Selectors;
import org.apache.commons.vfs2.impl.StandardFileSystemManager;
import org.apache.commons.vfs2.provider.sftp.IdentityInfo;
import org.apache.commons.vfs2.provider.sftp.SftpFileSystemConfigBuilder;

/**
 * based on
 * http://www.codejava.net/java-se/networking/ftp/java-ftp-file-download-tutorial-and-example
 */
public class SFTPKeyClientUpload {

	public static void main(String[] args) {

		try {
			String server = "localhost";
			int port = 2222;
			String user = "sftpuser";

			StandardFileSystemManager manager = new StandardFileSystemManager();

			// check if the file exists
			String filepath = "SFTP_UPLOADED_WITH_KEY.txt";
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
					System.getProperty("user.home") + "/.ssh_keys/simple-sftp");
			IdentityInfo identities = new IdentityInfo(new File(sshDir + "/sftpuser_key"), // Private key
					new File(sshDir + "/sftpuser_key.pub"), // Public key
					null // Passphrase
			);

			SftpFileSystemConfigBuilder.getInstance().setIdentityInfo(opts, identities);

			// Create the SFTP URI using the host name, userid, no password, remote path and
			// file name
			String sftpUri = "sftp://" + user + ":@" + server + ":2222" + "/data/" + filepath;

			// Create local file object
			FileObject localFile = manager.resolveFile(file.getAbsolutePath());

			// Create remote file object
			FileObject remoteFile = manager.resolveFile(sftpUri, opts);

			// Copy local file to sftp server
			remoteFile.copyFrom(localFile, Selectors.SELECT_SELF);
			System.out.println("File upload successful");

		} catch (FileSystemException e) {
			System.out.println(e.toString());
			e.printStackTrace();
		}
	}
}
