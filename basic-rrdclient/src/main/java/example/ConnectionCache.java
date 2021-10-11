package example;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.Map;

public class ConnectionCache {
	private Map<InetSocketAddress, ServerContext> channelMap = new HashMap<InetSocketAddress, ServerContext>();

	private void put(InetSocketAddress server, ServerContext context) {
		synchronized (channelMap) {
			channelMap.put(server, context);
		}
	}

	public void remove(InetSocketAddress server) {
		try {
			ServerContext context;
			synchronized (channelMap) {
				context = channelMap.get(server);
			}
			if (context != null) {
				context.close();
				synchronized (channelMap) {
					channelMap.remove(server);
				}
			}

		} catch (IOException e) {
		}
	}

	public SocketChannel get(InetSocketAddress server, long inactivityTimeout) throws IOException {
		ServerContext context = channelMap.get(server);
		if (context == null) {
			context = new ServerContext(server);
			put(server, context);
		} else {
			long currentTime = System.currentTimeMillis();
			if (context.getLastUsed() < currentTime - inactivityTimeout) {
				context.close();
				context = new ServerContext(server);
			} else {
				context.updateLastUsed(currentTime);
			}
		}
		return context.getChannel();
	}

	private class ServerContext {
		private SocketChannel channel;
		private long lastUsed;

		public ServerContext(InetSocketAddress server) throws IOException {
			channel = SocketChannel.open(server);
			lastUsed = System.currentTimeMillis();
		}

		public SocketChannel getChannel() {
			return channel;
		}

		public long getLastUsed() {
			return lastUsed;
		}

		public void updateLastUsed(long currentTime) {
			lastUsed = currentTime;
		}

		public void close() throws IOException {
			channel.close();
		}
	}
}
