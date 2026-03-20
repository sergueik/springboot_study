package example.configuration;

import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.consumer.ConsumerRecord;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.TopicPartition;
import org.apache.kafka.common.serialization.StringDeserializer;
import org.apache.kafka.common.serialization.StringSerializer;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.kafka.core.*;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.kafka.listener.DefaultErrorHandler;
import org.springframework.kafka.listener.DeadLetterPublishingRecoverer;

import org.springframework.util.backoff.FixedBackOff;

import example.service.KafkaService;

import java.util.HashMap;
import java.util.Map;

@Configuration
// NOTE: cannot name class "Configuration" because the Configuration is reserved and should be an annotation type
public class KafkaConfiguration {

	@Bean
	public ProducerFactory<String, String> producerFactory(KafkaProperties kafkaProperties) {

		Map<String, Object> config = new HashMap<>(kafkaProperties.buildProducerProperties());
		config.put(ProducerConfig.KEY_SERIALIZER_CLASS_CONFIG, StringSerializer.class);
		config.put(ProducerConfig.VALUE_SERIALIZER_CLASS_CONFIG, StringSerializer.class);

		return new DefaultKafkaProducerFactory<>(config);
	}

	@Bean
	public KafkaTemplate<String, String> kafkaTemplate(ProducerFactory<String, String> producerFactory) {
		return new KafkaTemplate<>(producerFactory);
	}

	@Bean
	public ConsumerFactory<String, String> consumerFactory(KafkaProperties kafkaProperties) {

		Map<String, Object> config = new HashMap<>(kafkaProperties.buildConsumerProperties());
		config.put(ConsumerConfig.KEY_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);
		config.put(ConsumerConfig.VALUE_DESERIALIZER_CLASS_CONFIG, StringDeserializer.class);

		return new DefaultKafkaConsumerFactory<>(config);
	}

	private static final Logger log = LoggerFactory.getLogger(DeadLetterPublishingRecoverer.class);

	@Bean
	public DefaultErrorHandler errorHandler(KafkaTemplate<String, String> template) {

		DeadLetterPublishingRecoverer recoverer = new DeadLetterPublishingRecoverer(template,
				(ConsumerRecord<?, ?> record, Exception ex) -> {
					log.info("DLQ publish: {}", record.value());
					return new TopicPartition(record.topic() + ".DLT", record.partition());
				});
		return new DefaultErrorHandler(recoverer);
	}

	@Bean
	public ConcurrentKafkaListenerContainerFactory<String, String> kafkaListenerContainerFactory(
			ConsumerFactory<String, String> consumerFactory, DefaultErrorHandler errorHandler) {

		ConcurrentKafkaListenerContainerFactory<String, String> factory = new ConcurrentKafkaListenerContainerFactory<>();

		factory.setConsumerFactory(consumerFactory);

		return factory;
	}
}
