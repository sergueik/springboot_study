package example.service;

import com.fasterxml.jackson.databind.ObjectMapper;
import example.dto.MessageDto;
import org.springframework.data.redis.connection.Message;
import org.springframework.data.redis.connection.MessageListener;
import org.springframework.stereotype.Service;

import java.io.IOException;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Service
public class DemoMessageListener implements MessageListener {

	private static final Logger log = LoggerFactory
			.getLogger(DemoMessageListener.class);

	private final ObjectMapper objectMapper;

	public DemoMessageListener(ObjectMapper objectMapper) {
		this.objectMapper = objectMapper;
	}

	@Override
	public void onMessage(Message message, byte[] pattern) {
		try {
			MessageDto msg = objectMapper.readValue(message.getBody(),
					MessageDto.class);
			if (msg != null) {
				log.info("Channel: {}, Message: {}", new String(message.getChannel()),
						msg.getBody());
			}
		} catch (IOException e) {
			log.error("Couldn't convert json", e);
		}
	}
}
