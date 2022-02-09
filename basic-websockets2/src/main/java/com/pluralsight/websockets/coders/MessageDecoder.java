package com.pluralsight.websockets.coders;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

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
				final Gson gson = new GsonBuilder().create();

				switch (type) {
				case MessageType.JOIN:

					message = gson.fromJson(msg, JoinMessage.class);
					// message = mapper.readValue(msg, JoinMessage.class);
					break;
				case MessageType.MESSAGE:
					message = gson.fromJson(msg, ChatMessage.class);
					// message = mapper.readValue(msg, ChatMessage.class);
					break;
				case MessageType.GETUSERS:
					message = gson.fromJson(msg, GetUsersMessage.class);
					// message = mapper.readValue(msg, GetUsersMessage.class);
					break;
				default:
					System.out.println(String.format("Unknown message type found [%s]",type));
					break;
				}
			} catch (Exception e) {
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
