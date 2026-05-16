package com.rodosaenz.ftp.client;

import java.io.File;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileSystemOptions;
import org.apache.commons.vfs2.Selectors;
import org.apache.commons.vfs2.impl.StandardFileSystemManager;
import org.apache.commons.vfs2.provider.sftp.IdentityInfo;
import org.apache.commons.vfs2.provider.sftp.SftpFileSystemConfigBuilder;

/**
 *
 * @author Rodolfo
 */
public class SFTPKeyClientUpload {

    public static void main(String[] args) {

        try {
            String server = "localhost";
            int port = 2222;
            String user = "sftpuser";

            StandardFileSystemManager manager = new StandardFileSystemManager();

            //check if the file exists
            String filepath = "SFTP_UPLOADED_WITH_KEY.txt";
            File file = new File(filepath);
            if (!file.exists()) {
                throw new RuntimeException("Error. Local file not found");
            }

            //Initializes the file manager
            manager.init();

            //Setup our SFTP configuration
            FileSystemOptions opts = new FileSystemOptions();
            SftpFileSystemConfigBuilder.getInstance().setStrictHostKeyChecking(
                    opts, "no");
            SftpFileSystemConfigBuilder.getInstance().setUserDirIsRoot(opts, true);
            SftpFileSystemConfigBuilder.getInstance().setTimeout(opts, 10000);


	    
	    
            IdentityInfo identites = new IdentityInfo(
                    new File("/home/sergueik/.ssh_keys/simple-sftp/sftpuser_key"), // Private-Key (OpenSSH Format)
                    new File("/home/sergueik/.ssh_keys/simple-sftp/sftpuser_key.pub"), // Public-Key (OpenSSH Format)
                    null); // Passphrase
            SftpFileSystemConfigBuilder.getInstance().setIdentityInfo(opts, identites);

            //Create the SFTP URI using the host name, userid, no password,  remote path and file name
            String sftpUri = "sftp://" + user + ":@" + server + ":2222" + "/data/" + filepath;

            // Create local file object
            FileObject localFile = manager.resolveFile(file.getAbsolutePath());

            // Create remote file object
            FileObject remoteFile = manager.resolveFile(sftpUri, opts);

            // Copy local file to sftp server
            remoteFile.copyFrom(localFile, Selectors.SELECT_SELF);
            System.out.println("File upload successful");

        } catch (FileSystemException e) {
		// org.apache.commons.vfs2.FileSystemException: Badly formed URI "sftp://sftpuser@localhost/share/SFTP_UPLOADED_WITH_KEY.txt".
		System.out.println(e.toString());
		e.printStackTrace();
        }
    }
}
