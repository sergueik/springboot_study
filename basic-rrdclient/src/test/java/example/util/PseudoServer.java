package example.util;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.concurrent.Executor;
import java.util.concurrent.Executors;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class PseudoServer implements Runnable {

	private static final Logger logger = LoggerFactory.getLogger(PseudoServer.class);

	protected int port;
	protected ServerSocketChannel serverChannel;
	private Executor threadPool = Executors.newCachedThreadPool();

	public PseudoServer(int port) throws IOException {
		logger.debug("Creating new server instance");
		this.port = port;
		serverChannel = ServerSocketChannel.open();
		serverChannel.socket().bind(new InetSocketAddress(port));
		Thread acceptThread = new Thread(this, "accept-thread:port:" + port);
		acceptThread.start();
	}

	public void run() {
		while (true) {
			try {
				acceptLoop();
			} catch (IOException e) {
				if (logger.isDebugEnabled())
					e.printStackTrace();
			}
		}
	}

	private void acceptLoop() throws IOException {
		SocketChannel clientChannel = serverChannel.accept();
		logger.debug("Connection established : " + clientChannel);
		threadPool.execute(new Responder(clientChannel));
	}

	private class Responder implements Runnable {
		private SocketChannel clientChannel;

		public Responder(SocketChannel clientChannel) {
			this.clientChannel = clientChannel;
		}

		public void run() {
			try {
				String request;
				BufferedReader in = new BufferedReader(new InputStreamReader(clientChannel.socket().getInputStream()));
				while ((request = in.readLine()) != null) {
					logger.debug("Received request : " + request);
					ByteBuffer response = respond(request);
					clientChannel.write(response);
					logger.debug("Finished sending response : " + response.limit() + " bytes");
				}
			} catch (IOException e) {
				logger.error(e.getMessage());
			}
		}
	}

	protected abstract ByteBuffer respond(String request);
}
