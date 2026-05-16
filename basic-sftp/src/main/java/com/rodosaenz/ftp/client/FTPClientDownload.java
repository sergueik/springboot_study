package com.rodosaenz.ftp.client;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import org.apache.commons.net.ftp.FTP;
import org.apache.commons.net.ftp.FTPClient;

/**
 * http://www.codejava.net/java-se/networking/ftp/java-ftp-file-download-tutorial-and-example
 *
 * @author Rodolfo
 */
public class FTPClientDownload {

    public static void main(String[] args) {

        String server = "myserver.example.com";
        int port = 21;
        String user = "username";
        String pass = "password";

        FTPClient ftpClient = new FTPClient();

        try {
            ftpClient.connect(server, port);
            ftpClient.login(user, pass);
            ftpClient.enterLocalPassiveMode();
            ftpClient.setFileType(FTP.BINARY_FILE_TYPE);

            // APPROACH #1: using retrieveFile(String, OutputStream)
            String filepath1 = "FTP_DOWNLOADED1.txt";
            String remoteFile1 = "/test/" + filepath1;
            File downloadFile1 = new File(filepath1);
            OutputStream outputStream1 = new BufferedOutputStream(new FileOutputStream(downloadFile1));
            boolean success = ftpClient.retrieveFile(remoteFile1, outputStream1);
            outputStream1.close();
            //downloadFile1.delete();

            if (success) {
                System.out.println("File #1 has been downloaded successfully.");
            }

            // APPROACH #2: using InputStream retrieveFileStream(String)
            String filepath2 = "FTP_DOWNLOADED2.txt";
            String remoteFile2 = "/test/" + filepath2;
            File downloadFile2 = new File(filepath2);
            OutputStream outputStream2 = new BufferedOutputStream(new FileOutputStream(downloadFile2));
            InputStream inputStream = ftpClient.retrieveFileStream(remoteFile2);
            byte[] bytesArray = new byte[4096];
            int bytesRead = -1;
            while ((bytesRead = inputStream.read(bytesArray)) != -1) {
                outputStream2.write(bytesArray, 0, bytesRead);
            }

            success = ftpClient.completePendingCommand();
            if (success) {
                System.out.println("File #2 has been downloaded successfully.");
            }
            outputStream2.close();
            inputStream.close();
            //downloadFile2.delete();

        } catch (IOException ex) {
            ex.printStackTrace();
        } finally {
            try {
                if (ftpClient.isConnected()) {
                    ftpClient.logout();
                    ftpClient.disconnect();
                }
            } catch (IOException ex) {
                ex.printStackTrace();
            }
        }
    }
}
