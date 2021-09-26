package com.pluralsight.websockets.coders;

import javax.websocket.EncodeException;
import javax.websocket.Encoder;
import javax.websocket.EndpointConfig;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.pluralsight.websockets.message.Message;

public class MessageEncoder implements Encoder.Text<Message>{

	@Override
	public String encode(Message message) throws EncodeException {
		ObjectMapper mapper = new ObjectMapper();
		try {
			return mapper.writeValueAsString(message);
		} catch (JsonProcessingException e) {
			e.printStackTrace();
			return "";
		}
	}

	@Override
	public void init(EndpointConfig config) {
	}

	@Override
	public void destroy() {
	}
}
