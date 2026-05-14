package example.messaging;

/**
 * Copyright 2020-2023 Serguei Kouzmine
 */


import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.JsonPath;
import com.neovisionaries.ws.client.WebSocket;
import com.neovisionaries.ws.client.WebSocketAdapter;
import com.neovisionaries.ws.client.WebSocketException;
import com.neovisionaries.ws.client.WebSocketFactory;

import example.utils.SSLUtil;
import example.utils.Utils;

// [ERROR]  package com.sun.xml.internal.ws.protocol.xml does not exist
// import com.sun.xml.internal.ws.protocol.xml.XMLMessageException;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.IOException;
import java.util.Base64;
import java.util.Objects;
import java.util.concurrent.*;

public class CDPClient {

	private String webSocketUrl = null;
	private WebSocket webSocket = null;
	private WebSocketFactory webSocketFactory;
	private boolean debug = false;
	private final int capacity = 100000;
	private final BlockingQueue<String> blockingQueue = new LinkedBlockingDeque<String>(
			capacity);
	private final long pollTimeout = 5;
	private int max_retry = 3;
	private JSONObject jsonObject = null;
	private JSONArray jsonArray = null;

	public void setDebug(boolean value) {
		debug = value;
	}

	public void setMaxRetry(int value) {
		max_retry = value;
	}

	public int getMaxRetry() {
		return max_retry;
	}

	public void setWebSocketUrl(String data) {
		webSocketUrl = data;
	}

	public CDPClient(String webSocketUrl) {
		webSocketFactory = new WebSocketFactory();
		SSLUtil.turnOffSslChecking(webSocketFactory);
		webSocketFactory.setVerifyHostname(false);
		this.webSocketUrl = webSocketUrl;
	}

	private void connect() throws IOException, WebSocketException {
		if (Objects.isNull(webSocket)) {
			 if (debug) {
				System.err.println("Making the new WS connection to: " + webSocketUrl);
			 }
		if (Objects.isNull(webSocketUrl)) {
				System.err.println("Abort Making the new WS connection to: " + webSocketUrl);
				return;
		}	
			webSocket = webSocketFactory.createSocket(webSocketUrl)
					.addListener(new WebSocketAdapter() {
						@Override
						public void onTextMessage(WebSocket webSocket, String message) {
							// Received a response. Print the received message.
							// TODO: support debug flag
							if (debug) {
								System.err.println("Received this ws message: " + message);
							}
							blockingQueue.add(message);
							if (debug) {
								System.err
										.println("message queue size: " + blockingQueue.size());
							}
						}
					}).connect();
		}
	}

	public void sendMessage(String message)
			throws IOException, WebSocketException {
		if (Objects.isNull(webSocket))
			this.connect();
		if (Objects.isNull(webSocket)){
			System.err.println("Cannot connect");
			return;
		}
		if (debug) {
			System.err.println("Sending this ws message: " + message);
		}
		webSocket.sendText(message);
	}

	public String getResponseMessage(String jsonPath, String expectedValue)
			throws InterruptedException {
		while (true) {
			String message = blockingQueue.poll(this.pollTimeout, TimeUnit.SECONDS);
			if (Objects.isNull(message)) {
				System.err.println("got no messages in queue.");
				return null;
			}
			if (debug)
				System.err.println("got raw message: " + message);
			DocumentContext parse = JsonPath.parse(message);
			String value = parse.read(jsonPath.trim()).toString();
			if (value.equalsIgnoreCase(expectedValue))
				return message;
		}
	}

	public String getResponseMessage(String methodName)
			throws InterruptedException {
		return getResponseMessage(methodName, this.pollTimeout);
	}

	public String getResponseMessage(String methodName, long pollTimeout)
			throws InterruptedException {
		try {
			while (true) {
				String message = blockingQueue.poll(pollTimeout, TimeUnit.SECONDS);
				if (Objects.isNull(message))
					throw new RuntimeException(String.format(
							"No message received with method name: \"%s\"", methodName));
				JSONObject jsonObject = new JSONObject(message);
				try {
					String method = jsonObject.getString("method");
					if (method.equalsIgnoreCase(methodName)) {
						return message;
					}
				} catch (JSONException e) {
					// do nothing
				}
			}
		} catch (Exception e1) {
			throw new RuntimeException(e1);
		}
	}

	public String getResponseBodyMessage(int id) throws InterruptedException {
		try {
			while (true) {
				// TODO: setter
				String message = blockingQueue.poll(pollTimeout, TimeUnit.SECONDS);
				if (debug) {
					System.err.println("message: " + message);

				}
				if (Objects.isNull(message))
					throw new RuntimeException(
							String.format("No message received with id: %s", id));
				jsonObject = new JSONObject(message);
				try {
					int methodId = jsonObject.getInt("id");
					if (id == methodId) {
						return jsonObject.getJSONObject("result").getString("body");
					}
				} catch (JSONException e) {
					// do nothing
				}
			}
		} catch (Exception e1) {
			throw new RuntimeException(e1);
		}
	}

	public String getResponseDataMessage(int id)
			throws InterruptedException, MessageTimeOutException {
		return getResponseMessage(id, "data");
	}

	public String getResponseMessage(int id, String dataType)
			throws InterruptedException, MessageTimeOutException {
		int retry_cnt = max_retry;
		while (retry_cnt >= 0) {
			String message = blockingQueue.poll(pollTimeout /* 10 */ ,
					TimeUnit.SECONDS);
			retry_cnt--;
			if (debug) {
				System.err.println("retry_cnt:" + retry_cnt + " message: " + message);

			}
			if (Objects.isNull(message)) {
				if (retry_cnt == 0) {
					throw new MessageTimeOutException(
							String.format("No message received with id : %s", id));
				}
			} else {
				jsonObject = new JSONObject(message);
				try {
					int methodId = jsonObject.getInt("id");
					if (id == methodId) {
						JSONObject resultObject = jsonObject.getJSONObject("result");
						if (debug) {
							System.err
									.println("processing result: " + resultObject.toString());
						}
						if (dataType == null) {
							return resultObject.toString();
						} else {
							String result = null;
							try {
								result = resultObject.getJSONArray(dataType).toString();
								if (debug)
									System.err.println("returning result: " + result);
								return result;
							} catch (JSONException e2) {
								if (debug) {
									System.err.println("failed to find array " + dataType
											+ " in the result: " + resultObject.toString());
								}
							}
							try {
								result = resultObject.getJSONObject(dataType).toString();
								if (debug) {
									System.err.println("returning result: " + result);
								}
								return result;
							} catch (JSONException e2) {
								if (debug) {
									System.err.println("failed to find object " + dataType
											+ " in the result: " + resultObject.toString());
								}
							}
							try {
								result = resultObject.getString(dataType);
								if (debug) {
									System.err.println("returning result: " + result);
								}
								return result;
							} catch (JSONException e2) {
								if (debug) {
									System.err.println("failed to find string " + dataType
											+ " in the result: " + resultObject.toString());
								}
							}
							try {
								result = String.format("%d", resultObject.getInt(dataType));
								if (debug) {
									System.err.println("returning result: " + result);
								}
								return result;
							} catch (JSONException e2) {
								if (debug) {
									System.err.println("failed to find integer " + dataType
											+ " in the result: " + resultObject.toString());
								}
							}

						}
					}
				} catch (JSONException e) {

					// do nothing
					// we may be hearing unrelated messages
					// TODO: distinguisn no "result" from no "result/dataType"
					/*
					 * throw new MessageTimeOutException(String.format(
					 * "Failed to parse JSON in the message with id %d: %s", id, message));
					 */
				}
			}
		}
		return null;
	}

	public void mockResponse(String mockMessage) {
		new Thread(() -> {
			try {
				String message = this.getResponseMessage("Network.requestIntercepted",
						5);
				jsonObject = new JSONObject(message);
				String interceptionId = jsonObject.getJSONObject("params")
						.getString("interceptionId");
				int id = Utils.getInstance().getDynamicID();
				this.sendMessage(
						MessageBuilder.buildGetContinueInterceptedRequestMessage(id,
								interceptionId, mockMessage));
				return;
			} catch (Exception e) {
				// do nothing
			}
		}).start();
	}

	public void mockFunResponse(String encodedMessage) {
		new Thread(() -> {
			try {
				while (true) {
					String message = this.getResponseMessage("Network.requestIntercepted",
							10);
					jsonObject = new JSONObject(message);
					String interceptionId = jsonObject.getJSONObject("params")
							.getString("interceptionId");
					// int id1 = Utils.getInstance().getDynamicID();
					// this.sendMessage(MessageBuilder.buildGetResponseBodyForInterceptionMessage(id1,interceptionId));
					// String interceptedResponse = this.getResponseBodyMessage(id1);
					int id = Utils.getInstance().getDynamicID();
					this.sendMessage(
							MessageBuilder.buildGetContinueInterceptedRequestEncodedMessage(
									id, interceptionId, encodedMessage));
				}
			} catch (Exception e) {
				// do nothing
			}
		}).start();
	}

	public ServiceWorker getServiceWorker(String workerURL, int timeoutInSecs,
			String expectedStatus) throws InterruptedException {
		while (true) {
			String message = getResponseMessage("ServiceWorker.workerVersionUpdated",
					timeoutInSecs);
			if (Objects.isNull(message))
				return null;
			jsonObject = new JSONObject(message);
			jsonArray = jsonObject.getJSONObject("params").getJSONArray("versions");
			try {
				String scriptURL = jsonArray.getJSONObject(0).getString("scriptURL");
				String status = jsonArray.getJSONObject(0).getString("status");
				if (scriptURL.contains(workerURL)
						&& status.equalsIgnoreCase(expectedStatus)) {
					String targetId = jsonArray.getJSONObject(0).getString("targetId");
					String versionId = jsonArray.getJSONObject(0).getString("versionId");
					String registrationId = jsonArray.getJSONObject(0)
							.getString("registrationId");
					String runningStatus = jsonArray.getJSONObject(0)
							.getString("registrationId");
					ServiceWorker serviceWorker = new ServiceWorker(versionId,
							registrationId, targetId);
					serviceWorker.setRunningStatus(runningStatus);
					serviceWorker.setStatus(status);
					return serviceWorker;
				}
			} catch (Exception e) {
				// do nothing
			}
		}
	}

	public void disconnect() {
		webSocket.disconnect();
	}

	@SuppressWarnings("serial")
	public static class MessageTimeOutException extends Exception {
		public MessageTimeOutException() {
			super();
		}

		public MessageTimeOutException(String message) {
			super(message);
		}

		public MessageTimeOutException(String message, Throwable cause) {
			super(message, cause);
		}
	}

}
