package example;

import org.apache.kafka.clients.producer.*;

import java.text.*;
import java.util.*;

public class ProducerApp {

	private final static String hostname = "localhost";
	private final static String topic = "test-topic";

	public static void main(String[] args) {

		final Properties properties = new Properties();
		properties.put("bootstrap.servers", String.format("%s:9092", hostname));
		properties.put("key.serializer",
				"org.apache.kafka.common.serialization.StringSerializer");
		properties.put("value.serializer",
				"org.apache.kafka.common.serialization.StringSerializer");
		properties.put("acks", "-1");
		properties.put("buffer.memory", 33554432);
		properties.put("compression.type", "none");
		properties.put("retries", 0);
		properties.put("batch.size", 16384);
		properties.put("client.id", "");
		properties.put("linger.ms", 0);
		properties.put("max.block.ms", 60000);
		properties.put("max.request.size", 1048576);
		properties.put("partitioner.class",
				"org.apache.kafka.clients.producer.internals.DefaultPartitioner");
		properties.put("request.timeout.ms", 30000);
		properties.put("timeout.ms", 30000);
		properties.put("max.in.flight.requests.per.connection", 5);
		properties.put("retry.backoff.ms", 5);

		final KafkaProducer<String, String> producer = new KafkaProducer<>(
				properties);

		System.out
				.println("created kafka producer: " + producer.getClass().getName());

		int maxCnt = 100;
		long interval = 1000;

		System.out.println("producing messages");
		for (int cnt = 0; cnt != maxCnt; cnt++) {
			try {
				producer.send(new ProducerRecord<String, String>(topic,
						String.format("Message # %s", Integer.toString(cnt))));
				System.out.println(
						String.format("Message # %s sent at %s", Integer.toString(cnt),
								new SimpleDateFormat("yyyy/MM/dd HH:mm:ss:SSS")
										.format(new Date())));
				Thread.sleep(interval);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		producer.close();
		System.out.println("done");

	}
}
