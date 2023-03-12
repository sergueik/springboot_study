package example.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.data.redis.connection.lettuce.LettuceConnectionFactory;
import org.springframework.data.redis.core.RedisTemplate;
import org.springframework.data.redis.listener.ChannelTopic;
import org.springframework.data.redis.listener.RedisMessageListenerContainer;
import org.springframework.data.redis.listener.adapter.MessageListenerAdapter;
import org.springframework.data.redis.serializer.StringRedisSerializer;

import example.service.DemoMessageListener;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Configuration
public class RedisConfig {
	@Value("${redis.pubsub.channel.name}")
	private String channel;
	private static final Logger log = LoggerFactory.getLogger(RedisConfig.class);

	@Bean
	public LettuceConnectionFactory lettuceConnectionFactory() {
		return new LettuceConnectionFactory();
	}

	@Bean
	public RedisTemplate<String, Object> redisTemplate(
			LettuceConnectionFactory lettuceConnectionFactory) {
		RedisTemplate<String, Object> template = new RedisTemplate<>();
		template.setDefaultSerializer(new StringRedisSerializer());
		template.setConnectionFactory(lettuceConnectionFactory);
		return template;
	}

	@Bean
	public RedisMessageListenerContainer redisMessageListenerContainer(
			LettuceConnectionFactory lettuceConnectionFactory,
			ChannelTopic channelTopic,
			MessageListenerAdapter messageListenerAdapter) {
		RedisMessageListenerContainer container = new RedisMessageListenerContainer();
		container.setConnectionFactory(lettuceConnectionFactory);
		container.addMessageListener(messageListenerAdapter, channelTopic);

		return container;
	}

	@Bean
	public MessageListenerAdapter demoMessageAdapter(
			DemoMessageListener demoMessageListener) {
		return new MessageListenerAdapter(demoMessageListener);
	}

	@Bean
	public ChannelTopic channelTopic() {
		return ChannelTopic.of(channel);
	}
}

