package example;

import org.apache.kafka.clients.consumer.*;

import java.time.Duration;
import java.util.*;

public class ConsumerGroupApp {

	private final static String hostname = "localhost";
	private final static String topic = "test-topic";

	@SuppressWarnings("rawtypes")
	public static void main(String[] args) {

		final Properties properties = new Properties();
		properties.put("bootstrap.servers", String.format("%s:9092", hostname));
		properties.put("key.deserializer", "org.apache.kafka.common.serialization.StringDeserializer");
		properties.put("value.deserializer", "org.apache.kafka.common.serialization.StringDeserializer");
		properties.put("fetch.min.bytes", 1);
		properties.put("group.id", "my-group");
		// required property when subscribing to topics
		properties.put("heartbeat.interval.ms", 3000);
		properties.put("max.partition.fetch.bytes", 1048576);
		properties.put("session.timeout.ms", 30000);
		properties.put("auto.offset.reset", "latest");
		properties.put("connections.max.idle.ms", 540000);
		properties.put("enable.auto.commit", true);
		properties.put("exclude.internal.topics", true);
		properties.put("max.poll.records", 2147483647);
		properties.put("partition.assignment.strategy", "org.apache.kafka.clients.consumer.RangeAssignor");
		properties.put("request.timeout.ms", 40000);
		properties.put("auto.commit.interval.ms", 5000);
		properties.put("fetch.max.wait.ms", 500);
		properties.put("metadata.max.age.ms", 300000);
		properties.put("reconnect.backoff.ms", 50);
		properties.put("retry.backoff.ms", 100);
		properties.put("client.id", "");

		final KafkaConsumer<String, String> consumer = new KafkaConsumer<>(properties);

		consumer.subscribe(Arrays.asList(new String[] { topic }));

		Set<String> subscribedTopics = consumer.subscription();

		printSet(subscribedTopics);

		try {
			while (true) {
				ConsumerRecords records = consumer.poll(1000);
				// the argument to poll is Duration.ofMillis(1000) in some Kafka
				// versions
				printRecords(records);
				consumer.commitAsync();
			}
		} finally {
			consumer.close();
		}

	}

	private static void printSet(Set<String> collection) {
		if (collection.isEmpty())
			System.out.println("not subscribed to any topics.");
		else
			System.out.println("subscribed to the following topics: " + String.join(",", collection));
	}

	private static void printRecords(ConsumerRecords<String, String> records) {
		for (ConsumerRecord<String, String> record : records) {
			System.out.println(String.format("Topic: %s, Partition: %d, Offset: %d, Key: %s, Value: %s", record.topic(),
					record.partition(), record.offset(), record.key(), record.value()));
		}
	}
}
