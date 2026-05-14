package example.messaging;

/**
 * Copyright 2020-2023 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import org.apache.commons.codec.binary.Base64;

import com.google.gson.Gson;

import example.utils.Utils;

public class MessageBuilder {

	private static String method = null;
	private static Message message = null;
	private static final boolean debug = true; 
	private static Map<String, Object> params = new HashMap<>();
	private static Map<String, Object> data = new HashMap<>();

	private static class Message {
		@SuppressWarnings("unused")
		private int id;
		@SuppressWarnings("unused")
		private String method;
		private Map<String, Object> params;
		private static final Gson gson = new Gson();

		public Message(int id, String method) {
			this.id = id;
			this.method = method;
		}

		public void addParam(String key, Object value) {
			if (Objects.isNull(params))
				params = new HashMap<>();
			params.put(key, value);
		}

		public String toJson() {
			return gson.toJson(this);
		}
	}

	private static String buildMessage(int id, String method) {
		return (new Message(id, method)).toJson();
	}

	private static String buildMessage(int id, String method,
			Map<String, Object> params) {
		message = new Message(id, method);
		for (String key : params.keySet()) {
			message.addParam(key, params.get(key));
		}
		return message.toJson();
	}

	public static String buildCustomMessage(int id, String method,
			Map<String, Object> params) {
		if (debug) {
			StringBuffer paramArg = new StringBuffer();
			for (String key : params.keySet()) {
				paramArg.append(String.format("\"%s\":%s, ", key, params.get(key)));
			}
			System.err.println(String.format(
					"Sending:\n" + "{\"id\":%d,\"method\":\"%s\"," + "\"params\":{ %s }}",
					id, method, paramArg.toString()));

		}
		return buildMessage(id, method, params);
	}

	public static String buildBrowserVersionMessage(int id) {
		return buildMessage(id, "Browser.getVersion");
	}

	public static String buildBrowserVersionMessage() {
		return buildMessage(Utils.getInstance().getDynamicID(),
				"Browser.getVersion");
	}
	public static String buildGetContinueInterceptedRequestMessage(int id,
			String interceptionId, String rawResponse) {
		method = "Network.getResponseBodyForInterception";
		params = new HashMap<>();
		params.put("interceptionId", interceptionId);
		params.put("rawResponse",
				new String(Base64.encodeBase64(rawResponse.getBytes())));
		return buildMessage(id, method, params);
	}

	public static String buildGetContinueInterceptedRequestEncodedMessage(int id,
			String interceptionId, String encodedResponse) {
		method = "Network.getResponseBodyForInterception";
		params = new HashMap<>();
		params.put("interceptionId", interceptionId);
		params.put("rawResponse", encodedResponse);
		return buildMessage(id, method, params);
	}


}
