package example;

import java.io.BufferedReader;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.StringTokenizer;

// vanilla HTTP Server
// origin: http://www.java2s.com/Code/Java/Tiny-Application/HttpServer.htm

public class HttpServer {
	public static void main(String args[]) {
		int port;
		ServerSocket server_socket;
		try {
			port = Integer.parseInt(args[0]);
		} catch (Exception e) {
			port = 1500;
		}
		try {
			server_socket = new ServerSocket(port);
			System.err.println("httpServer running on port " + server_socket.getLocalPort());

			// server infinite loop
			while (true) {
				Socket socket = server_socket.accept();
				System.err.println("New connection accepted " + socket.getInetAddress() + ":" + socket.getPort());

				// Construct handler to process the HTTP request message.
				try {
					httpRequestHandler request = new httpRequestHandler(socket);
					// Create a new thread to process the request.
					Thread thread = new Thread(request);

					// Start the thread.
					thread.start();
					// print and ignore all exceptions when processing requests
				} catch (Exception e) {
					System.err.println("Exception handling request:" + e.toString());
				}
			}
		} catch (IOException e) {
			System.err.println("Exception opening socket:" + e.toString());
			System.out.println(e);
		}
	}
}

class httpRequestHandler implements Runnable {
	final static String CRLF = "\r\n";

	Socket socket;
	InputStream input;
	OutputStream output;
	BufferedReader bufferedReader;
	String statusLine = null;
	String contentTypeLine = null;
	String errorPage = null;
	String contentLengthLine = null;
	FileInputStream fileInputStream = null;
	StringTokenizer stringTokenizer = null;
	String requestMethod = null;
	String filePath = null;
	boolean fileExists = true;
	final String serverLine = "Server: Simple Java Http Server";
	
	public httpRequestHandler(Socket socket) throws Exception {
		this.socket = socket;
		input = socket.getInputStream();
		output = socket.getOutputStream();
		bufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()));
	}

	public void run() {
		try {
			processRequest();
		} catch (Exception e) {
			System.err.println("Exception :" + e);
		}
	}

	private void processRequest() {

		while (true) {

			try {
				String line = bufferedReader.readLine();

				// debug log processing of each input request
				System.err.println("input: " + line);
				if (line.equals(CRLF) || line.equals(""))
					break;

				stringTokenizer = new StringTokenizer(line);
				requestMethod = stringTokenizer.nextToken();

				if (requestMethod.equals("GET")) {

					filePath = "." + stringTokenizer.nextToken();
					System.err.println("requested path: " + filePath);

					try {
						fileInputStream = new FileInputStream(filePath);
					} catch (FileNotFoundException e) {
						fileExists = false;
					}

					if (fileExists) {
						statusLine = "HTTP/1.0 200 OK" + CRLF;
						contentTypeLine = "Content-type: " + contentType(filePath) + CRLF;
						contentLengthLine = "Content-Length: " + (new Integer(fileInputStream.available())).toString()
								+ CRLF;
					} else {
						statusLine = "HTTP/1.0 404 Not Found" + CRLF;
						contentTypeLine = "text/html";
						errorPage = "<HTML>" + "<HEAD><TITLE>404 Not Found</TITLE></HEAD>" + "<BODY>404 Not Found"
								+ "<br>usage:http://yourHostName:port/" + "fileName.html</BODY></HTML>";
					}

					// HTTP status
					output.write(statusLine.getBytes());
					// Server
					output.write(serverLine.getBytes());
					// Content-type
					output.write(contentTypeLine.getBytes());
					// Content-Length
					output.write(contentLengthLine.getBytes());

					// blank line to indicate the end of the header
					output.write(CRLF.getBytes());
					// body
					if (fileExists) {
						sendBytes(fileInputStream, output);
						fileInputStream.close();
					} else {
						output.write(errorPage.getBytes());
					}
				}
			} catch (Exception e) {
				System.err.println("Exception (ignored) in processRequest:" + e);
			}
		}
		try {
			output.close();
			bufferedReader.close();
			socket.close();
		} catch (Exception e) {
			System.err.println("Exception (ignored) in processRequest:" + e);
		}
	}

	private static void sendBytes(FileInputStream fileInputStream, OutputStream outputStream) throws Exception {

		byte[] buffer = new byte[1024];
		int bytes = 0;

		while ((bytes = fileInputStream.read(buffer)) != -1) {
			outputStream.write(buffer, 0, bytes);
		}
	}

	private static String contentType(String fileName) {
		if (fileName.endsWith(".htm") || fileName.endsWith(".html") || fileName.endsWith(".txt")) {
			return "text/html";
		} else if (fileName.endsWith(".jpg") || fileName.endsWith(".jpeg")) {
			return "image/jpeg";
		} else if (fileName.endsWith(".gif")) {
			return "image/gif";
		} else {
			return "application/octet-stream";
		}
	}
}
