package example;

import org.apache.kafka.clients.producer.*;
import org.apache.kafka.common.serialization.StringSerializer;

import java.text.*;
import java.util.*;

public class ProducerApp {

	private final static String hostname = "localhost";
	private final static String topic = "test-topic";

	public static void main(String[] args) {

		final Properties properties = new Properties();
		properties.put("bootstrap.servers", String.format("%s:9092", hostname));
		// ack from leader and all "in-sync" replicas
		properties.put("acks", "all");
		properties.put("enable.idempotence", "1");

		// no security protocol specified in this demo
		// not providing "sasl.jaas.config" information for locally hosted kafka
		// cluster
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
				properties, new StringSerializer(), new StringSerializer());

		System.out
				.println("created kafka producer: " + producer.getClass().getName());

		int maxCnt = 100;
		long interval = 1000;

		System.out.println("producing messages");
		for (int cnt = 0; cnt != maxCnt; cnt++) {
			try {
				// send synchronously
				RecordMetadata metadata = producer
						.send(new ProducerRecord<String, String>(topic,
								String.format("Message # %s", Integer.toString(cnt))))
						.get();
				System.out.println(
						String.format("Message # %s sent at %s to partition %s offset %s",
								Integer.toString(cnt),
								new SimpleDateFormat("yyyy/MM/dd HH:mm:ss:SSS")
										.format(new Date()),
								Integer.toString(metadata.partition()),
								Long.toString(metadata.offset())));
				Thread.sleep(interval);
			} catch (Exception e) {
				e.printStackTrace();
			}
		}
		producer.close();
		System.out.println("done");

	}
}
