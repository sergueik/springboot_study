package example;

import com.rabbitmq.client.Channel;
import com.rabbitmq.client.Connection;
import com.rabbitmq.client.ConnectionFactory;

import java.io.IOException;
import java.net.URISyntaxException;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.util.concurrent.TimeoutException;

public class Publisher {

	public static void main(String[] argv)
			throws NoSuchAlgorithmException, KeyManagementException,
			URISyntaxException, IOException, InterruptedException, TimeoutException {
		ConnectionFactory factory = new ConnectionFactory();
		factory.setUri("amqp://guest:guest@localhost");
		factory.setConnectionTimeout(300000);
		Connection connection = factory.newConnection();
		Channel channel = connection.createChannel();

		channel.queueDeclare(argv[0], true, false, false, null);

		int count = 0;

		while (count < 10) {
			String message = "Message number " + count;

			channel.basicPublish("", argv[1], null, message.getBytes());
			count++;
			System.err.println("Published message: " + message);
			Thread.sleep(1000);
		}
	}
}
