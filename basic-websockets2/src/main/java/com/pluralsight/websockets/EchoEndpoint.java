package com.pluralsight.websockets;

import javax.websocket.*;
import javax.websocket.RemoteEndpoint.Basic;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;

@ServerEndpoint(value = "/echo")
public class EchoEndpoint {

	@OnOpen
	public void onOpen(Session session, EndpointConfig endpointConfig) {
		RemoteEndpoint.Basic remoteEndpointBasic = session.getBasicRemote();
		session.addMessageHandler(new EchoMessageHandler(remoteEndpointBasic));
	}

	static class EchoMessageHandler implements MessageHandler.Whole<String> {

		private final Basic remoteEndpointBasic;

		public EchoMessageHandler(Basic remoteEndpointBasic) {
			this.remoteEndpointBasic = remoteEndpointBasic;
		}

		@Override
		public void onMessage(String message) {
			if (remoteEndpointBasic != null) {
				try {
					System.out.println("Echoing : " + message + "\n");
					remoteEndpointBasic.sendText(String.format("echo :: %s", message));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}

	}
}
