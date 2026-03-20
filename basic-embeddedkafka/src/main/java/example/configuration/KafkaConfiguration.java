package example.configuration;

import org.apache.kafka.clients.consumer.ConsumerConfig;
import org.apache.kafka.clients.producer.ProducerConfig;
import org.apache.kafka.common.serialization.StringSerializer;
import org.apache.kafka.common.serialization.StringDeserializer;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import org.springframework.kafka.config.ConcurrentKafkaListenerContainerFactory;
import org.springframework.boot.autoconfigure.kafka.KafkaProperties;
import org.springframework.kafka.core.*;
import org.springframework.kafka.listener.SeekToCurrentErrorHandler;

import org.springframework.kafka.listener.DeadLetterPublishingRecoverer;

import org.springframework.util.backoff.FixedBackOff;

import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("deprecation")
@Configuration
// NOTE: cannot name class "Configuration" because 
// the name Configuration is reserved and should be an annotation type
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

	@Bean
	public ConcurrentKafkaListenerContainerFactory<String, String> kafkaListenerContainerFactory(
			ConsumerFactory<String, String> consumerFactory, KafkaTemplate<String, String> kafkaTemplate) {

		ConcurrentKafkaListenerContainerFactory<String, String> factory = new ConcurrentKafkaListenerContainerFactory<>();

		factory.setConsumerFactory(consumerFactory);

		// DLQ support
		DeadLetterPublishingRecoverer recoverer = new DeadLetterPublishingRecoverer(kafkaTemplate);
		SeekToCurrentErrorHandler errorHandler = new SeekToCurrentErrorHandler(recoverer, new FixedBackOff(0L, 0L));

		factory.setErrorHandler(errorHandler);
		return factory;
	}
}
