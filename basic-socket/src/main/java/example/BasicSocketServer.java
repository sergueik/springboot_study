package example;

import jnr.enxio.channels.NativeSelectorProvider;
import jnr.unixsocket.UnixServerSocketChannel;
import jnr.unixsocket.UnixSocketChannel;

import java.io.IOException;
import java.io.InputStreamReader;
import java.net.SocketException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.AlreadyBoundException;
import java.nio.channels.Channels;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.Set;

import static java.nio.charset.StandardCharsets.UTF_8;

public class BasicSocketServer {
	private static final String DATA = "message";

	private static UnixSocketPair socketPair;
	private static Thread server;
	private static volatile Exception serverException;

	public static void main(String args[]) {
		// Arrange
		try {
			socketPair = new UnixSocketPair();
			final UnixServerSocketChannel channel = UnixServerSocketChannel.open();
			final Selector sel = NativeSelectorProvider.getInstance().openSelector();
			channel.configureBlocking(false);
			channel.socket().bind(socketPair.socketAddress());
			channel.register(sel, SelectionKey.OP_ACCEPT, new ServerActor(channel, sel));

			server = new Thread("server side") {
				public void run() {
					try {
						while (sel.select() > 0) {
							Set<SelectionKey> keys = sel.selectedKeys();
							for (SelectionKey k : keys) {
								Actor a = (Actor) k.attachment();
								if (!a.rxready()) {
									k.cancel();
								}
							}
						}
					} catch (Exception ex) {
						serverException = ex;
					}
				}
			};

			server.setDaemon(true);
			server.start();
			try {
				sleepForever();
			} catch (InterruptedException e) {
				// TODO: cleanup
				return;
			}
			socketPair.close();
			if (serverException != null) {
				throw new RuntimeException(serverException.toString());
			}
		} catch (IOException e) {
			throw new RuntimeException(e.toString());
		}
	}

	static interface Actor {
		public boolean rxready();
	}

	public static void sleepForever() throws InterruptedException {
		while (true) {
			Thread.sleep(10000);
			Thread.yield();
		}
	}

	private static final class ServerActor implements Actor {
		private final UnixServerSocketChannel channel;
		private final Selector selector;

		public ServerActor(UnixServerSocketChannel channel, Selector selector) {
			this.channel = channel;
			this.selector = selector;
		}

		public final boolean rxready() {
			try {
				UnixSocketChannel client = channel.accept();

				if (client == null) {
					return false;
				}
				client.configureBlocking(false);
				client.register(selector, SelectionKey.OP_READ, new ClientActor(client));

				return true;
			} catch (IOException ex) {
				return false;
			}
		}
	}

	private static final class ClientActor implements Actor {
		private final UnixSocketChannel channel;

		public ClientActor(UnixSocketChannel channel) {
			this.channel = channel;
		}

		public final boolean rxready() {
			try {
				ByteBuffer buf = ByteBuffer.allocate(1024);
				int n = channel.read(buf);

				// assertEquals(DATA.length(), n);
				// message size assert not useful when communicating with socat
				if (n > 0) {
					buf.flip();
					System.err.println("Processed: " + new String(buf.array(), "UTF-8"));
					channel.write(buf);
					return true;
				} else if (n < 0) {
					return false;
				}

			} catch (IOException ex) {
				ex.printStackTrace();
				return false;
			}
			return true;
		}
	}
}
