package com.pluralsight.websockets.coders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.pluralsight.websockets.message.*;

import javax.json.Json;
import javax.json.JsonException;
import javax.json.JsonObject;
import javax.websocket.DecodeException;
import javax.websocket.Decoder;
import javax.websocket.EndpointConfig;
import java.io.IOException;
import java.io.StringReader;

public class MessageDecoder implements Decoder.Text<Message> {

	@Override
	public Message decode(String msg) throws DecodeException {
		Message message = null;

		if (willDecode(msg)) {
			try {
				int type = getMessageType(msg);
				ObjectMapper mapper = new ObjectMapper();

				switch (type) {
				case MessageType.JOIN:
					message = mapper.readValue(msg, JoinMessage.class);
					break;
				case MessageType.MESSAGE:
					message = mapper.readValue(msg, ChatMessage.class);
					break;
				case MessageType.GETUSERS:
					message = mapper.readValue(msg, GetUsersMessage.class);
					break;
				default:
					System.out.println(String.format("Unknown message type found [%s]",type));
					break;
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}

		return message;
	}

	private int getMessageType(String msg) {
		JsonObject obj = Json.createReader(new StringReader(msg)).readObject();
		int type = obj.getInt("type");
		return type;
	}

	@Override
	public boolean willDecode(String msg) {
		try {
			Json.createReader(new StringReader(msg));
			return true;
		} catch (JsonException e) {
			return false;
		}
	}

	@Override
	public void init(EndpointConfig config) {
	}

	@Override
	public void destroy() {
	}
}
