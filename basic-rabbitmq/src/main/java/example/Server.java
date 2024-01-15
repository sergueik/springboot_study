package example;

import com.rabbitmq.client.*;

import java.io.IOException;
import java.util.Random;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicInteger;

public class Server {

	private String ip;
	private int port;
	private String user;
	private String password;
	private String virtual_host;

	public String getIp() {
		return ip;
	}

	public void setIp(String value) {
		ip = value;
	}

	public int getPort() {
		return port;
	}

	public void setPort(int value) {
		port = value;
	}

	public String getUser() {
		return user;
	}

	public void setUser(String value) {
		user = value;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String value) {
		password = value;
	}

	public String getVirtual_host() {
		return virtual_host;
	}

	public void setVirtual_host(String value) {
		virtual_host = value;
	}

	public void setConnection(Connection connection) {
		this.connection = connection;
	}

	private Connection connection;

	public void init_connection() {

		ConnectionFactory factory = new ConnectionFactory();

		factory.setPassword(getPassword());
		factory.setUsername(getUser());
		factory.setHost(getIp());
		factory.setPort(getPort());
		factory.setVirtualHost(getVirtual_host());
		try {
			System.out.println(String.format(
					"Trying new connection with host: %s port: %d virtual host: %s",
					getIp(), getPort(), getVirtual_host()));
			this.connection = factory.newConnection();
			System.out.println("Connection done: " + getIp());

		} catch (IOException e) {
			e.printStackTrace();
		} catch (TimeoutException e) {
			e.printStackTrace();
		}

	}

	public void start_server() throws Exception {
		final String exchange_name = "my_company";
		final Channel channel = connection.createChannel();

		channel.exchangeDeclare(exchange_name, "topic", true);
		System.err.println("Created exchange " + exchange_name);
		final String store_messages_queue = "store.messages";
		channel.queueDeclare(store_messages_queue, true, false, false, null);
		System.err.println("Created queue " + store_messages_queue);
		channel.queueBind(store_messages_queue, exchange_name, "#");
		channel.basicQos(1);
		final AtomicInteger totalmessages = new AtomicInteger();

		channel.basicConsume(store_messages_queue, new DefaultConsumer(channel) {

			public void handleDelivery(String consumerTag, Envelope envelope,
					AMQP.BasicProperties properties, byte[] body) throws IOException {
				String message = new String(body, "UTF-8");

				System.err.println("processing message '" + message + "'..."
						+ envelope.getRoutingKey());
				try {
					Random random = new Random();
					int seconds = random.nextInt(10 - 5 + 1) + 5;
					System.err.print(" waiting ");

					for (int i = 0; i < seconds; i++) {
						System.out.print(".");
						Thread.sleep(1000);

					}

					System.out.println("Insert for '" + message + "' done");
				} catch (InterruptedException e) {
					e.printStackTrace();
				} finally {
					System.out.println("Precessed: " + totalmessages.incrementAndGet());
					channel.basicAck(envelope.getDeliveryTag(), false);
				}

			}

		});

	}

	public static void main(String[] argv) throws Exception {
		Server server = new Server();
		server.setIp(argv[0]);
		server.setPort(Integer.parseInt(argv[1]));
		server.setUser(argv[2]);
		server.setPassword(argv[3]);
		server.setVirtual_host(argv[4]);
		// final String exchange = argv[5];

		System.out.println("Starting server..");
		server.init_connection();
		server.start_server();
		System.out.println("Server started");
	}

}
