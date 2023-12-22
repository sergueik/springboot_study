package example;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;
import com.rabbitmq.client.DefaultConsumer;
import com.rabbitmq.client.DeliverCallback;
// https://github.com/IBMStreams/streamsx.messaging/issues/139
// what is the successor/replacement of QueueingConsumer?
// https://www.programcreek.com/java-api-examples/?api=com.rabbitmq.client.QueueingConsumer.Delivery
// import com.rabbitmq.client.QueueingConsumer;

import java.io.IOException;
import java.net.URISyntaxException;
import java.nio.charset.StandardCharsets;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.TimeoutException;

public class Consumer {
	public static void main(String[] argv)
			throws NoSuchAlgorithmException, KeyManagementException,
			URISyntaxException, IOException, InterruptedException, TimeoutException {
		ConnectionFactory factory = new ConnectionFactory();
		factory.setUri("amqp://guest:guest@localhost");
		factory.setConnectionTimeout(300000);
		Connection connection = factory.newConnection();
		Channel channel = connection.createChannel();
		final String queueName = argv[0];
		channel.queueDeclare(queueName, true, false, false, null);
		System.err.println(
				"Waiting for messages in: " + queueName + "\n" + "To exit press CTRL+C");

		channel.basicQos(1);
		DeliverCallback deliverCallback = (consumerTag, delivery) -> {
			String message = new String(delivery.getBody(), "UTF-8");

			System.err.println(" [x] Received '" + message + "'");
			try {
				doWork(message);
				channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
				System.err.println(" [x] Done");
			} catch (Exception e) {
				channel.basicReject(delivery.getEnvelope().getDeliveryTag(), true);
			}
		};
		channel.basicConsume(queueName, false, deliverCallback, consumerTag -> {
		});
		/*
		QueueingConsumer consumer = new QueueingConsumer(channel);
		channel.basicConsume(queueName, false, consumer);
		while (true) {
			QueueingConsumer.Delivery delivery = consumer.nextDelivery();
		
			if (delivery != null) {
				try {
					String message = new String(delivery.getBody(),
							StandardCharsets.UTF_8);
					System.out.println("Message consumed: " + message);
					// Interact with IO
					channel.basicAck(delivery.getEnvelope().getDeliveryTag(), false);
				} catch (Exception e) {
					channel.basicReject(delivery.getEnvelope().getDeliveryTag(), true);
				}
			}
		}
			*/

	}

	private static void doWork(String task) {
		try {
			Thread.sleep(1000);
		} catch (InterruptedException _ignored) {
			Thread.currentThread().interrupt();
		}
	}
}
