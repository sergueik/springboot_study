package example;

import org.apache.kafka.clients.producer.*;
import org.apache.kafka.common.serialization.StringSerializer;

import java.text.*;
import java.util.*;

public class BatchProducerApp {

	private final static String topic = "test-topic";

	public static void main(String[] args) {

		final Properties properties = new Properties();
		properties.put("bootstrap.servers", "localhost:9092, localhost:9093");
		properties.put("key.serializer", "org.apache.kafka.common.serialization.StringSerializer");
		properties.put("value.serializer", "org.apache.kafka.common.serialization.StringSerializer");
		// no ack
		properties.put("acks", "0");
		properties.put("buffer.memory", 33554432);
		properties.put("compression.type", "none");
		properties.put("retries", 0);
		properties.put("batch.size", 16384);
		properties.put("client.id", "");
		properties.put("linger.ms", 0);
		properties.put("max.block.ms", 60000);
		properties.put("max.request.size", 1048576);
		properties.put("partitioner.class", "org.apache.kafka.clients.producer.internals.DefaultPartitioner");
		properties.put("request.timeout.ms", 30000);
		properties.put("timeout.ms", 30000);
		properties.put("max.in.flight.requests.per.connection", 5);
		properties.put("retry.backoff.ms", 5);

		final KafkaProducer<String, String> producer = new KafkaProducer<>(properties);

		System.out.println("created kafka producer: " + producer.getClass().getName());

		try {
			int batchNumber = 1;
			int counter = 0;
			while (true) {
				do {

					System.out.println("created batch " + Integer.toString(batchNumber) + " record # "
							+ Integer.toString(counter));

					// send asynchronously
					producer.send(new ProducerRecord<String, String>(topic,
							String.format("Batch: %s", Integer.toString(batchNumber)), String.format("result from %s",
									new SimpleDateFormat("yyyy/MM/dd HH:mm:ss:SSS").format(new Date()))));
					counter++;
					Thread.sleep(500);
				} while (counter < 10);
				counter = 0;
				Thread.sleep(500);
				batchNumber++;
			}

		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			producer.close();
		}

	}
}
