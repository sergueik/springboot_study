package com.rodosaenz.ftp.client;

import java.io.File;
import org.apache.commons.vfs2.FileObject;
import org.apache.commons.vfs2.FileSystemException;
import org.apache.commons.vfs2.FileSystemOptions;
import org.apache.commons.vfs2.Selectors;
import org.apache.commons.vfs2.impl.StandardFileSystemManager;
import org.apache.commons.vfs2.provider.sftp.SftpFileSystemConfigBuilder;

/**
 * http://www.mysamplecode.com/2013/06/sftp-apache-commons-file-download.html
 *
 * @author Rodolfo
 */
public class SFTPClientDownload {

    public static void main(String[] args) {

        try {
            String server = "myserver.example.com";
            int port = 21;
            String user = "username";
            String pass = "password";

            StandardFileSystemManager manager = new StandardFileSystemManager();

            //check if the file exists
            String filepath = "SFTP_DOWNLOADED.txt";
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
            
            //Create the SFTP URI using the host name, userid, password,  remote path and file name
            String sftpUri = "sftp://" + user + ":" + pass + "@" + server + "/test/" + filepath;

            // Create local file object
            FileObject localFile = manager.resolveFile(file.getAbsolutePath());

            // Create remote file object
            FileObject remoteFile = manager.resolveFile(sftpUri, opts);

            // Copy local file from sftp server
            localFile.copyFrom(remoteFile, Selectors.SELECT_SELF);
            System.out.println("File download successful");
            
        } catch (FileSystemException ex) {
        }
    }
}
