package example.controller;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.event.EventListener;
import org.springframework.web.socket.WebSocketSession;
import org.springframework.messaging.MessageHeaders;
import org.springframework.messaging.simp.SimpMessageSendingOperations;
import org.springframework.messaging.simp.stomp.StompHeaderAccessor;
import org.springframework.web.socket.messaging.StompSubProtocolHandler;

import org.springframework.stereotype.Component;
import org.springframework.web.socket.messaging.SessionConnectedEvent;
import org.springframework.web.socket.messaging.SessionDisconnectEvent;

import example.model.ChatMessage;

@Component
public class WebSocketEventListener {

	private static final Logger logger = LoggerFactory
			.getLogger(WebSocketEventListener.class);

	@Autowired
	private SimpMessageSendingOperations messagingTemplate;

	@EventListener
	public void handleWebSocketConnectListener(SessionConnectedEvent event) {

		StompSubProtocolHandler sender = (StompSubProtocolHandler) event
				.getSource();
		String sessionId = sender.resolveSessionId(event.getMessage());
		// new MapSession(sessionId);
		logger.info("Received a new web socket connection: " + event.getMessage()
				+ " from session " + sessionId);
		// https://www.devglan.com/spring-boot/spring-session-stomp-websocket
		StompHeaderAccessor ha = StompHeaderAccessor.wrap(event.getMessage());
    user = SessionUtils.getUser(ha);
	}

	@EventListener
	public void handleWebSocketDisconnectListener(SessionDisconnectEvent event) {
		StompHeaderAccessor headerAccessor = StompHeaderAccessor
				.wrap(event.getMessage());

		String username = (String) headerAccessor.getSessionAttributes()
				.get("username");
		if (username != null) {
			logger.info("User Disconnected : " + username);

			ChatMessage chatMessage = new ChatMessage();
			chatMessage.setType(ChatMessage.MessageType.LEAVE);
			chatMessage.setSender(username);
			messagingTemplate.convertAndSend("/topic/public", chatMessage);
		}
	}
}
