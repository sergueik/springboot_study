package example;

import jnr.enxio.channels.NativeSelectorProvider;
import jnr.unixsocket.UnixServerSocketChannel;
import jnr.unixsocket.UnixSocketChannel;

import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

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
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

public class BasicFunctionalityTest {
	private static final String DATA = "message";
	// private static final boolean useExternalClient =
	// Boolean.getBoolean(System.getenv("EXTERNALCLIENT"));
	private static final boolean useExternalClient = (System.getenv("EXTERNAL_CLIENT") != null
			|| System.getProperty("useExternalClient", null) != null);

	private final int timeout = 120000;
	private UnixSocketPair socketPair;
	private Thread server;
	private volatile Exception serverException;

	@Before
	public void setUp() throws Exception {
		socketPair = new UnixSocketPair();
	}

	@After
	public void tearDown() throws Exception {
		socketPair.close();
	}

	@Test
	public void basicOperation() throws Exception {
		// Arrange
		// server logic
		System.err.println("useExternalClient: " + useExternalClient);
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

						assertNotNull(keys);
						assertTrue(keys.size() > 0);

						for (SelectionKey k : keys) {
							assertTrue(k.attachment() instanceof Actor);

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

		if (useExternalClient) {
			server.setDaemon(true);
		}
		server.start();
		if (useExternalClient) {
			try {
				System.err.println("Sleep: " + timeout + " m/sec");
				Thread.sleep(timeout);
			} catch (InterruptedException e) {
			}
		} else {
			// client logic
			UnixSocketChannel channel2 = UnixSocketChannel.open(socketPair.socketAddress());

			assertEquals(socketPair.socketAddress(), channel2.getRemoteSocketAddress());

			Channels.newOutputStream(channel2).write(DATA.getBytes(UTF_8));
			System.err.println("Sent: " + DATA);

			InputStreamReader r = new InputStreamReader(Channels.newInputStream(channel2), UTF_8);
			CharBuffer result = CharBuffer.allocate(1024);
			r.read(result);

			assertEquals(DATA.length(), result.position());

			result.flip();

			assertEquals(DATA, result.toString());
			System.err.println("Received: " + result.rewind());

		}
		if (serverException != null) {
			throw serverException;
		}
	}

	static interface Actor {
		public boolean rxready();
	}

	final class ServerActor implements Actor {
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
				assertEquals(socketPair.socketAddress(), client.getLocalSocketAddress());
				client.configureBlocking(false);
				client.register(selector, SelectionKey.OP_READ, new ClientActor(client));

				return true;
			} catch (IOException ex) {
				return false;
			}
		}
	}

	final class ClientActor implements Actor {
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
