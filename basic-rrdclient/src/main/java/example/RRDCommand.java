package example;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class RRDCommand {
	private static final Logger logger = LoggerFactory.getLogger(RRDCommand.class);
	private static ConnectionCache cache = new ConnectionCache();
	protected InetAddress serverAddress;

	public void setServerAddress(String value) {
		try {
			serverAddress = InetAddress.getByName(value);
		} catch (UnknownHostException e) {
			throw new IllegalArgumentException(e);
		}
	}

	protected int serverPort;

	public void setServerPort(int value) {
		serverPort = value;
	}

	protected int inactivityTimeout = 600000;

	public void setInactivityTimeout(int value) {
		inactivityTimeout = value;
	}

	protected double userTime, systemTime;
	String returnCode;

	public abstract void execute(OutputStream out);

	public abstract void execute();

	public void execute(OutputStream out, String address, int port) {
		setServerAddress(address);
		setServerPort(port);
		execute(out);
	}

	public void execute(String address, int port) {
		setServerAddress(address);
		setServerPort(port);
		execute();
	}

	protected ByteBuffer sendCommandToServer(String command) throws IOException {
		InetSocketAddress server = new InetSocketAddress(serverAddress, serverPort);
		try {
			boolean errorFound = false;
			command += "\n";
			SocketChannel channel = cache.get(server, inactivityTimeout);
			ByteBuffer receiveBuffer = ByteBuffer.allocate(1024 * 1024);
			synchronized (channel) {
				channel.socket().setSoTimeout(60000);
				if (logger.isDebugEnabled()) {
					logger.debug("Sending command : " + command);
				}
				ByteBuffer sendBuffer = ByteBuffer.wrap(command.getBytes());

				int bytesWritten = channel.write(sendBuffer);
				if (logger.isTraceEnabled())
					logger.trace("Sent " + bytesWritten + " bytes to " + server);

				boolean stillSomethingToRead = true;
				while (stillSomethingToRead) {
					logger.trace("There is still something to read");
					int bytesRead;
					try {
						bytesRead = channel.read(receiveBuffer);
					} catch (SocketTimeoutException e) {
						throw new RRDToolError("Socket timed out");
					}
					if (logger.isTraceEnabled())
						logger.trace("Received " + bytesRead + " bytes from " + server);
					int position = receiveBuffer.position();
					if (receiveBuffer.get(--position) == '\n') {
						int lineStart = 0;
						while (position > 0) {
							byte previousByte = receiveBuffer.get(--position);
							if (previousByte == 'O') {
								lineStart = position;
								break;
							}
						}
						if (receiveBuffer.get(lineStart) == 'O' && receiveBuffer.get(lineStart + 1) == 'K'
								&& receiveBuffer.get(lineStart + 2) == ' ') {
							stillSomethingToRead = false;
							logger.trace("Found OK");
						}
						if (receiveBuffer.get(lineStart) == 'O' && receiveBuffer.get(lineStart + 1) == 'R'
								&& receiveBuffer.get(lineStart + 2) == ':') {
							stillSomethingToRead = false;
							logger.trace("Found ERROR");
							errorFound = true;
						}
					}
				}
			}

			receiveBuffer.flip();
			if (logger.isTraceEnabled())
				logger.trace("Received a total of " + receiveBuffer.limit() + " bytes from " + server);
			if (errorFound) {
				byte[] messageBuffer = new byte[receiveBuffer.limit()];
				receiveBuffer.get(messageBuffer);
				throw new RRDToolError(new String(messageBuffer));
			}
			return receiveBuffer;
		} catch (IOException e) {
			cache.remove(server);
			throw e;
		}
	}

	protected void verifyIfError(String line) {
		if (line.startsWith("ERROR")) {
			throw new RuntimeException(line);
		}
	}

	protected void parseStatusLine(String line) {
		returnCode = "OK";
		String[] explodedLine = line.split("[ :]+");
		userTime = Double.parseDouble(explodedLine[2]);
		systemTime = Double.parseDouble(explodedLine[4]);
		if (logger.isTraceEnabled()) {
			logger.trace("Return code : " + returnCode + "\n" + "System time : " + systemTime + "\n" + "User time : "
					+ userTime);
		}
	}
}
