package example;

import org.apache.kafka.clients.producer.*;
import org.apache.kafka.common.serialization.StringSerializer;

import java.text.*;
import java.util.*;

public class ProducerApp {

	private final static String hostname = "192.168.0.113";
	private final static String topic = "test-topic";
	private final static String username = "user";
	private final static String password = "78S3fBujz9NB";

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
		properties.put("security.protocol", "SASL_PLAINTEXT");
		properties.put("jaas.enabled", "true");
		properties.put("sasl.mechanism", "PLAIN");
		properties.put("sasl.jaas.config",
				String.format(
						"org.apache.kafka.common.security.plain.PlainLoginModule required username='%s' password='%s';",
						username, password));

		final KafkaProducer<String, String> producer = new KafkaProducer<>(
				properties, new StringSerializer(), new StringSerializer());
		// Caused by: java.lang.IllegalArgumentException: Could not find a
		// 'KafkaClient' entry in the JAAS configuration.
		// System property 'java.security.auth.login.config' is not set
		// see also:

		// https://github.com/CloudKarafka/java-kafka-example/blob/master/src/main/java/KafkaExample.java
		// https://commandstech.com/solvedcould-not-find-a-kafkaclient-entry-in-the-jaas-configuration-in-kafka-cluster-kafkaerror/
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
