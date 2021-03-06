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
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

// vanilla HTTP Server
// origin: http://www.java2s.com/Code/Java/Tiny-Application/HttpServer.htm

public class HttpServer {
	public static void main(String args[]) {
		ServerSocket server_socket;
		int port = (args.length > 0) ? Integer.parseInt(args[0]) : 8500;
		try {
			server_socket = new ServerSocket(port);
			System.err.println(
					"httpServer running on port " + server_socket.getLocalPort());

			// server infinite loop
			while (true) {
				Socket socket = server_socket.accept();
				System.err.println("New connection accepted " + socket.getInetAddress()
						+ ":" + socket.getPort());

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
	final String serverLine = "Server: Simple Java Http Server" + CRLF;
	StringBuilder headerLine = new StringBuilder();

	public httpRequestHandler(Socket socket) throws Exception {
		this.socket = socket;
		input = socket.getInputStream();
		output = socket.getOutputStream();
		bufferedReader = new BufferedReader(
				new InputStreamReader(socket.getInputStream()));
	}

	public void run() {
		try {
			processRequest();
		} catch (Exception e) {
			System.err.println("Exception: " + e);
		}
	}

	private void processRequest() {
		Header header = new Header(null);
		while (true) {
			try {
				String line = bufferedReader.readLine();
				if ((line.equals(CRLF) || line.equals(""))) {
					// done with headers
					if (fileExists) {
						statusLine = "HTTP/1.0 200 OK" + CRLF;
						contentTypeLine = "Content-type: " + contentType(filePath) + CRLF;
						contentLengthLine = "Content-Length: "
								+ (new Integer(fileInputStream.available())).toString() + CRLF;
						// not generating the traceID
						// https://www.baeldung.com/java-method-reflection

						for (final Method method : header.getClass().getMethods()) {
							final String methodName = method.getName();
							// System.err.println("inspecting method: " + methodName);
							if (methodName.startsWith("get")
									&& method.getParameters().length == 0) {
								String field = methodName.substring("get".length())
										.toLowerCase();
								// https://docs.oracle.com/javase/tutorial/reflect/member/methodInvocation.html
								System.err.println("getting value of field: " + field);
								Object val = method.invoke(header);
								if (val == null) {
									System.err.println("Ignoring null value of: " + field);
								} else {
									System.err.println(
											"Adding to response headers " + field + ": " + val);
									headerLine.append(field + ": " + val.toString() + CRLF);
								}
							}
						}

					} else {
						statusLine = "HTTP/1.0 404 Not Found" + CRLF;
						contentTypeLine = "Content-type: text/html" + CRLF;
						errorPage = "<HTML>" + "<HEAD><TITLE>404 Not Found</TITLE></HEAD>"
								+ "<BODY>404 Not Found" + "<br>usage:http://yourHostName:port/"
								+ "fileName.html</BODY></HTML>";
						contentLengthLine = String.format("Content-Length: %d%s",
								errorPage.length(), CRLF);
					}

					// HTTP status
					output.write(statusLine.getBytes());
					// Server
					output.write(serverLine.getBytes());
					// Content-type
					output.write(contentTypeLine.getBytes());
					// Content-Length
					output.write(contentLengthLine.getBytes());

					if (headerLine != null) {
						// custom headers
						System.err
								.println("Sending custom header: " + headerLine.toString());
						output.write(headerLine.toString().getBytes());
					}
					// blank line to indicate the end of the header
					output.write(CRLF.getBytes());
					// body
					if (fileExists) {
						System.err.println("returning page");
						sendBytes(fileInputStream, output);
						fileInputStream.close();
					} else {
						System.err.println("returning error page");
						output.write(errorPage.getBytes());
					}
					break;
				} else {
					requestMethod = null;
					try {
						stringTokenizer = new StringTokenizer(line);
						requestMethod = stringTokenizer.nextToken();
						// debug log processing of each input request
						System.err.println("input: " + line + " request: " + requestMethod);
					} catch (java.util.NoSuchElementException e) {
						// ignore
					}
					if (requestMethod.equals("GET")) {

						filePath = "." + stringTokenizer.nextToken();
						System.err.println("requested path: " + filePath);

						try {
							fileInputStream = new FileInputStream(filePath);
						} catch (FileNotFoundException e) {
							System.err.println("Exception (processed): " + e);
							fileExists = false;
						}
					}
					// hack to detect and pass through traceId header
					if (requestMethod.matches("^traceid.*")) {
						String data = stringTokenizer.nextToken();
						System.err
								.println("detect and pass through traceid header: " + data);
						header.setTraceID(data);
					}
				}
			} catch (Exception e) {
				System.err.println("Exception (ignored) in processRequest:" + e);
			}
		}
		try

		{
			output.close();
			bufferedReader.close();
			socket.close();
		} catch (Exception e) {
			System.err.println("Exception (ignored) in processRequest:" + e);
		}
	}

	private static void sendBytes(FileInputStream fileInputStream,
			OutputStream outputStream) throws Exception {

		byte[] buffer = new byte[1024];
		int bytes = 0;

		while ((bytes = fileInputStream.read(buffer)) != -1) {
			outputStream.write(buffer, 0, bytes);
		}
	}

	private static String contentType(String fileName) {
		if (fileName.endsWith(".htm") || fileName.endsWith(".html")
				|| fileName.endsWith(".txt")) {
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
