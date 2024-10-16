package example;

import org.apache.kafka.common.*;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.clients.consumer.*;

import java.util.*;

public class ConsumerApp {

	private final static String hostname = "192.168.0.113";
	private final static String topic = "test-topic";

	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static void main(String[] args) {

		final Properties properties = new Properties();
		properties.put("bootstrap.servers", String.format("%s:9092", hostname));
		properties.put("fetch.min.bytes", 1);
		properties.put("group.id", "");
		properties.put("heartbeat.interval.ms", 3000);
		properties.put("max.partition.fetch.bytes", 1048576);
		properties.put("session.timeout.ms", 30000);
		properties.put("auto.offset.reset", "latest");
		properties.put("connections.max.idle.ms", 540000);
		properties.put("enable.auto.commit", true);
		properties.put("exclude.internal.topics", true);
		properties.put("max.poll.records", 2147483647);
		properties.put("partition.assignment.strategy",
				"org.apache.kafka.clients.consumer.RangeAssignor");
		properties.put("request.timeout.ms", 40000);
		properties.put("auto.commit.interval.ms", 5000);
		properties.put("fetch.max.wait.ms", 500);
		properties.put("metadata.max.age.ms", 300000);
		properties.put("reconnect.backoff.ms", 50);
		properties.put("retry.backoff.ms", 100);
		properties.put("client.id", "");

		properties.put("username", "user");
		properties.put("password", "78S3fBujz9NB");
		KafkaConsumer<String, String> consumer = new KafkaConsumer<>(properties,
				new StringDeserializer(), new StringDeserializer());

		ArrayList<TopicPartition> partitions = new ArrayList<>();
		partitions.add(new TopicPartition(topic, 0));
		// partitions.add(new TopicPartition(topic, 1));
		consumer.assign(partitions);

		Set<TopicPartition> assignedPartitions = consumer.assignment();

		printSet(assignedPartitions);

		try {
			while (true) {
				ConsumerRecords records = consumer.poll(1000);
				printRecords(records);
			}
		} finally {
			consumer.close();
		}

	}

	private static void printSet(Set<TopicPartition> collection) {
		if (collection.isEmpty()) {
			System.out.println("not assigned to any partitions");
		} else {
			System.out.println("assigned to partitions:");
			for (TopicPartition partition : collection) {
				System.out.println(String.format("Partition: %s in Topic: %s",
						Integer.toString(partition.partition()), partition.topic()));
			}
		}
	}

	private static void printRecords(ConsumerRecords<String, String> records) {
		for (ConsumerRecord<String, String> record : records) {
			System.out.println(String.format(
					"Topic: %s, Partition: %d, Offset: %d, Key: %s, Value: %s",
					record.topic(), record.partition(), record.offset(), record.key(),
					record.value()));
		}
	}
}
