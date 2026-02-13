package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.IOException;
import java.io.InterruptedIOException;
import java.net.ServerSocket;
import java.net.Socket;

import org.apache.http.ConnectionClosedException;
import org.apache.http.HttpConnectionFactory;
import org.apache.http.HttpException;
import org.apache.http.HttpServerConnection;
import org.apache.http.impl.DefaultBHttpServerConnection;
import org.apache.http.impl.DefaultBHttpServerConnectionFactory;
import org.apache.http.impl.NoConnectionReuseStrategy;
import org.apache.http.protocol.BasicHttpContext;
import org.apache.http.protocol.HttpContext;
import org.apache.http.protocol.HttpProcessor;
import org.apache.http.protocol.HttpProcessorBuilder;
import org.apache.http.protocol.HttpService;
import org.apache.http.protocol.ResponseConnControl;
import org.apache.http.protocol.ResponseContent;
import org.apache.http.protocol.ResponseDate;
import org.apache.http.protocol.ResponseServer;
import org.apache.http.protocol.UriHttpRequestHandlerMapper;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// based on: https://github.com/omerio/graphviz-server/blob/master/src/info/dawelbeit/graphviz/dot/DotGraphics.java 

public class DotGraphics {

	private static final Logger log = LoggerFactory.getLogger(DotGraphics.class);
	private final int port;
	private Thread requestListenerThread;

	public DotGraphics(int port) {
		this.port = port;
	}

	public void start() throws IOException {
		HttpProcessor httpproc = HttpProcessorBuilder.create().add(new ResponseDate())
				.add(new ResponseServer("DotGraphics/1.1")).add(new ResponseContent()).add(new ResponseConnControl())
				.build();

		UriHttpRequestHandlerMapper reqistry = new UriHttpRequestHandlerMapper();
		reqistry.register("*", new HttpDotGraphMessageHandler());

		HttpService httpService = new HttpService(httpproc, NoConnectionReuseStrategy.INSTANCE, null, reqistry, null);

		requestListenerThread = new RequestListenerThread(port, httpService);
		requestListenerThread.setDaemon(false);
		requestListenerThread.start();
		GraphViz.init();
	}

	public void stop2() throws InterruptedException {
		try {
			((DotGraphics.RequestListenerThread) requestListenerThread).getServerSocket().close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		requestListenerThread.interrupt();
	}

	public void stop() {
		if (requestListenerThread instanceof RequestListenerThread) {
			RequestListenerThread listener = (RequestListenerThread) requestListenerThread;
			try {
				listener.getServerSocket().close();
				Thread.sleep(1000);
			} catch (InterruptedException e) {
				// ignore
			} catch (IOException e) {
				log.warn("Failed to close server socket", e);
			} finally {
				listener.interrupt();
			}
		}
	}

	public static void main(String[] args) throws Exception {
		if (args.length != 1) {
			System.err.println("Usage java -jar DotGraphics.jar <port>");
			System.exit(1);
		}
		int port = Integer.parseInt(args[0]);
		new DotGraphics(port).start();
	}

	static class RequestListenerThread extends Thread {

		private final HttpConnectionFactory<DefaultBHttpServerConnection> connFactory;
		private final ServerSocket serversocket;
		private final HttpService httpService;

		public ServerSocket getServerSocket() {
			return this.serversocket;
		}

		public RequestListenerThread(final int port, final HttpService httpService) throws IOException {
			this.connFactory = DefaultBHttpServerConnectionFactory.INSTANCE;
			this.serversocket = new ServerSocket(port);
			this.httpService = httpService;
		}

		@Override
		public void run() {
			log.info("Listening on port " + this.serversocket.getLocalPort());
			while (!Thread.interrupted()) {
				try {
					Socket socket = this.serversocket.accept();
					log.info("Incoming connection from " + socket.getInetAddress());
					HttpServerConnection conn = this.connFactory.createConnection(socket);

					Thread t = new WorkerThread(this.httpService, conn);
					t.setDaemon(true);
					t.start();

				} catch (InterruptedIOException ex) {
					break;
				} catch (IOException e) {
					log.error("I/O error initialising connection thread: " + e.getMessage(), e);
					break;
				}
			}
		}
	}

	static class WorkerThread extends Thread {

		private final HttpService httpService;
		private final HttpServerConnection httpServerConnection;

		public WorkerThread(final HttpService httpService, final HttpServerConnection httpServerConnection) {
			super();
			this.httpService = httpService;
			this.httpServerConnection = httpServerConnection;
		}

		@Override
		public void run() {
			log.info("New connection thread");
			HttpContext context = new BasicHttpContext(null);
			try {
				while (!Thread.interrupted() && this.httpServerConnection.isOpen()) {
					this.httpService.handleRequest(this.httpServerConnection, context);
				}
			} catch (ConnectionClosedException e) {
				log.error("Client closed connection {}", e.getMessage(), e);

			} catch (IOException e) {
				log.error("I/O error: {}", e.getMessage(), e);

			} catch (HttpException e) {
				log.error("Unrecoverable HTTP protocol violation: {}", e.getMessage(), e);

			} finally {
				try {
					this.httpServerConnection.shutdown();
				} catch (IOException ignore) {
				}
			}
		}

	}

}
