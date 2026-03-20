package example;

import org.apache.kafka.clients.consumer.Consumer;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.consumer.ConsumerRecords;
import org.apache.kafka.clients.consumer.ConsumerConfig;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import org.springframework.kafka.core.ConsumerFactory;
import org.springframework.kafka.core.DefaultKafkaConsumerFactory;
import org.springframework.kafka.core.KafkaTemplate;
import org.springframework.kafka.test.context.EmbeddedKafka;
import org.springframework.kafka.test.utils.KafkaTestUtils;
import org.springframework.test.annotation.DirtiesContext;
import org.springframework.test.context.DynamicPropertyRegistry;
import org.springframework.test.context.DynamicPropertySource;
import org.springframework.test.context.TestPropertySource;

import org.springframework.kafka.config.KafkaListenerEndpointRegistry;
import org.springframework.kafka.listener.MessageListenerContainer;
import org.springframework.kafka.test.EmbeddedKafkaBroker;

import org.apache.kafka.common.serialization.StringDeserializer;

// import static org.junit.jupiter.api.Assertions.*;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.hasItems;

import java.util.Map;
import java.util.Set;
import java.util.Collections;
import java.time.Duration;

@SpringBootTest(classes = Application.class)
@EmbeddedKafka(partitions = 1, topics = { "input-topic", "input-topic.DLT" })
@TestPropertySource(properties = { "spring.kafka.bootstrap-servers=${spring.embedded.kafka.brokers}" })
@DirtiesContext
class KafkaIntegrationTest {

	@Autowired
	private KafkaTemplate<String, String> kafkaTemplate;

	@Autowired
	private ConsumerFactory<String, String> consumerFactory;

	@Autowired
	private EmbeddedKafkaBroker embeddedKafka;

	private Consumer<String, String> consumer = null;
	private ConsumerRecord<String, String> record = null;

	@BeforeEach
	public void beforeEach() {

		Map<String, Object> consumerProps = KafkaTestUtils.consumerProps("test-group" + System.currentTimeMillis(),
				"true", embeddedKafka);
		consumerProps.put(ConsumerConfig.AUTO_OFFSET_RESET_CONFIG, "earliest");

		ConsumerFactory<String, String> consumerFactory = new DefaultKafkaConsumerFactory<>(consumerProps,
				new StringDeserializer(), new StringDeserializer());

		consumer = consumerFactory.createConsumer();

	}

	@DisplayName("get message")
	@Test
	void test1() {

		consumer.subscribe(Collections.singleton("input-topic"));
		kafkaTemplate.send("input-topic", "good-message");

		record = KafkaTestUtils.getSingleRecord(consumer, "input-topic");

		assertThat(record, notNullValue());
		assertThat(record.value(), is("good-message"));
	}

	@DisplayName("Bad message goes to DLQ")
	@Test
	void test2() {

		// Step 1: subscribe first
		consumer.subscribe(Collections.singleton("input-topic.DLT"));

		// Step 2: send message that will fail
		kafkaTemplate.send("input-topic", "bad-message");
		Set<String> topics = embeddedKafka.getTopics();
		assertThat(topics, hasItems(new String[] { "input-topic", "input-topic.DLT" }));

		// Step 3: poll until record appears (with timeout)
		ConsumerRecord<String, String> record = null;

		long timeout = System.currentTimeMillis() + 5000;
		boolean found = false;
		while (System.currentTimeMillis() < timeout) {
			ConsumerRecords<String, String> records = consumer.poll(Duration.ofMillis(200));
			// assertThat(records, notNullValue());
			// assertThat(records.isEmpty(), is(false));

			if (records != null && !records.isEmpty()) {
				record = records.iterator().next();
				found = true;
				break;
			}
		}
		assertThat(found, is(true));
		assertThat(record, notNullValue());
		assertThat(record.value(), is("bad-message"));
	}
}
