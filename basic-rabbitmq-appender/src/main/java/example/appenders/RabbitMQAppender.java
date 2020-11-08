package example.appenders;

import com.rabbitmq.client.AMQP;
import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

import org.apache.log4j.AppenderSkeleton;
import org.apache.log4j.spi.ErrorCode;
import org.apache.log4j.spi.LoggingEvent;

import java.io.IOException;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeoutException;

public class RabbitMQAppender extends AppenderSkeleton {

	private ConnectionFactory factory = new ConnectionFactory();
	private Connection connection = null;
	private Channel channel = null;
	private String identifier = null;
	private String host = "localhost";
	private int port = 5762;
	private String username = "guest";
	private String password = "guest";
	private String virtualHost = "/";
	private String exchange = "amqp-exchange";
	private String type = "direct";
	private boolean durable = false;
	private String queue = "amqp-queue";
	private String routingKey = "";

	private ExecutorService threadPool = Executors.newSingleThreadExecutor();

	@Override
	protected void append(LoggingEvent loggingEvent) {
		if (isAsSevereAsThreshold(loggingEvent.getLevel())) {
			threadPool.submit(new AppenderTask(loggingEvent));
		}
	}

	@Override
	public void activateOptions() {
		super.activateOptions();

		try {
			createConnection();
		} catch (IOException | TimeoutException e) {
			errorHandler.error(e.getMessage(), e, ErrorCode.GENERIC_FAILURE);
		}

		try {
			createChannel();
		} catch (IOException e) {
			errorHandler.error(e.getMessage(), e, ErrorCode.GENERIC_FAILURE);
		}

		try {
			createExchange();
		} catch (Exception e) {
			errorHandler.error(e.getMessage(), e, ErrorCode.GENERIC_FAILURE);
		}
		try {
			createQueue();
		} catch (Exception e) {
			errorHandler.error(e.getMessage(), e, ErrorCode.GENERIC_FAILURE);
		}
	}

	private void setFactoryConfiguration() {
		factory.setHost(host);
		factory.setPort(port);
		factory.setVirtualHost(virtualHost);
		factory.setUsername(username);
		factory.setPassword(password);
	}

	public String getIdentifier() {
		return identifier;
	}

	public void setIdentifier(String value) {
		identifier = value;
	}

	public String getHost() {
		return host;
	}

	public void setHost(String value) {
		host = value;
	}

	public int getPort() {
		return port;
	}

	public void setPort(int value) {
		port = value;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String value) {
		username = value;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String value) {
		password = value;
	}

	public String getVirtualHost() {
		return virtualHost;
	}

	public void setVirtualHost(String value) {
		virtualHost = value;
	}

	public String getExchange() {
		return exchange;
	}

	public void setExchange(String value) {
		exchange = value;
	}

	public String getType() {
		return type;
	}

	public void setType(String value) {
		type = value;
	}

	public String getQueue() {
		return queue;
	}

	public void setQueue(String value) {
		queue = value;
	}

	public boolean isDurable() {
		return durable;
	}

	public void setDurable(boolean value) {
		durable = value;
	}

	public String getRoutingKey() {
		return routingKey;
	}

	public void setRoutingKey(String value) {
		routingKey = value;
	}

	private void createExchange() throws IOException {
		if (channel != null && channel.isOpen()) {
			synchronized (channel) {
				channel.exchangeDeclare(exchange, type, durable);
			}
		}
	}

	private void createQueue() throws IOException {
		if (channel != null && channel.isOpen()) {
			synchronized (channel) {
				channel.queueDeclare(queue, false, false, false, null);
				channel.queueBind(queue, exchange, routingKey);
			}
		}
	}

	private Channel createChannel() throws IOException {
		if (channel == null || !channel.isOpen() && (connection != null && connection.isOpen())) {
			channel = connection.createChannel();
		}
		return channel;
	}

	private Connection createConnection() throws IOException, TimeoutException {
		setFactoryConfiguration();
		if (connection == null || !connection.isOpen()) {
			connection = factory.newConnection();
		}
		return connection;
	}

	@Override
	public void close() {
		if (channel != null && channel.isOpen()) {
			try {
				channel.close();
			} catch (IOException | TimeoutException ioe) {
				errorHandler.error(ioe.getMessage(), ioe, ErrorCode.CLOSE_FAILURE);
			}
		}

		if (connection != null && connection.isOpen()) {
			try {
				connection.close();
			} catch (IOException e) {
				errorHandler.error(e.getMessage(), e, ErrorCode.CLOSE_FAILURE);
			}
		}
	}

	@Override
	public boolean requiresLayout() {
		return true;
	}

	class AppenderTask implements Callable<LoggingEvent> {
		LoggingEvent loggingEvent;

		AppenderTask(LoggingEvent data) {
			loggingEvent = data;
		}

		@Override
		public LoggingEvent call() throws Exception {
			String payload = layout.format(loggingEvent);
			String id = String.format("%s:%s", identifier, System.currentTimeMillis());

			AMQP.BasicProperties.Builder b = new AMQP.BasicProperties().builder();
			b.appId(identifier).type(loggingEvent.getLevel().toString()).correlationId(id).contentType("text/json");

			createChannel().basicPublish(exchange, routingKey, b.build(), payload.toString().getBytes());

			return loggingEvent;
		}
	}
}
