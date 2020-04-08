package example;

import java.io.File;
import java.io.IOException;
import java.net.Socket;
import java.util.UUID;

import jnr.unixsocket.UnixServerSocketChannel;
import jnr.unixsocket.UnixSocketAddress;
import jnr.unixsocket.UnixSocketChannel;

class UnixSocketPair extends TestSocketPair {
	static String socketDir = getEnv("socket_dir", "/tmp");
	static final Factory FACTORY = new Factory() {
		@Override
		TestSocketPair createUnconnected() throws IOException {
			return new UnixSocketPair();
		}
	};

	private final File file;
	private final UnixSocketAddress address;
	private final String fileName = socketDir + "/" + "test" + ((int) (Math.random() * 100)) + ".sock";
	private UnixServerSocketChannel serverSocketChannel;
	private UnixSocketChannel serverChannel;
	private UnixSocketChannel clientChannel;
	private final int timeout = 10000;

	UnixSocketPair() throws IOException {
		file = new File(fileName);
		address = new UnixSocketAddress(file);
		serverSocketChannel = UnixServerSocketChannel.open();
		System.err.println("Opened socket: " + fileName);
	}

	@Override
	void serverBind() throws IOException {
		serverSocketChannel.configureBlocking(true);
		serverSocketChannel.socket().bind(address);
	}

	@Override
	void clientConnect() throws IOException {
		if (clientChannel != null) {
			throw new IllegalStateException("already connected");
		}

		clientChannel = UnixSocketChannel.open();
		clientChannel.connect(new UnixSocketAddress(file));
	}

	@Override
	void serverAccept() throws IOException {
		if (serverChannel != null) {
			throw new IllegalStateException("already accepted");
		}

		serverChannel = serverSocketChannel.accept();
	}

	@Override
	UnixSocketAddress socketAddress() {
		return address;
	}

	@Override
	Socket server() {
		return serverChannel.socket();
	}

	@Override
	Socket client() {
		return clientChannel.socket();
	}

	@Override
	public void close() throws IOException {
		closeQuietly(serverSocketChannel);
		closeQuietly(serverChannel);
		closeQuietly(clientChannel);
		file.delete();
	}

	public static String getEnv(String name, String defaultValue) {
		String value = System.getenv(name);
		if (value == null || value.length() == 0) {
			value = defaultValue;
		}

		return value;
	}
}
