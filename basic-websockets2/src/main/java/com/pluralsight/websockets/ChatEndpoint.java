package com.pluralsight.websockets;

import com.pluralsight.websockets.coders.MessageDecoder;
import com.pluralsight.websockets.coders.MessageEncoder;
import com.pluralsight.websockets.message.*;

import javax.websocket.*;
import javax.websocket.server.PathParam;
import javax.websocket.server.ServerEndpoint;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CopyOnWriteArrayList;

@ServerEndpoint(value = "/chat/{room-name}", decoders = MessageDecoder.class, encoders = MessageEncoder.class)
public class ChatEndpoint {

    private static Map<String, List<ChatEndpoint>> clients = new HashMap<>();
    private static Map<String, List<User>> users = new HashMap<>();
    private static Map<String, List<ChatMessage>> messages = new HashMap<>();

    private Session session;
    private String roomName;

    @OnError
    public void onError(Session session, Throwable t, @PathParam("room-name") String roomName) {
        try {
            session.close();
        } catch (IOException e) {
            System.err.println(String.format("Error closing socket for room [%s]", roomName));
            e.printStackTrace();
        }
        System.err.println(String.format("Error raised for room [%s]", roomName));
        t.printStackTrace();
    }

    @OnOpen
    public void onOpen(Session session, EndpointConfig _, @PathParam("room-name") String roomName) {
        this.session = session;
        this.roomName = roomName;
        List<ChatEndpoint> roomClients = getClientsForRoom();
        roomClients.add(this);
    }

    private List<ChatEndpoint> getClientsForRoom() {
        return ChatEndpoint.clients.computeIfAbsent(roomName, s -> new CopyOnWriteArrayList<>()); // java 8 style
    }

    @OnClose
    public void onClose(Session session, CloseReason reason, @PathParam("room-name") String roomName) {
        if (!clients.containsKey(roomName)) {
            throw new IllegalArgumentException(String.format("Valid room name expected, got [%s]", roomName));
        }

        System.out.println(
                String.format("Socket closed for room [%s] : [%s], reason: %s",
                        roomName,
                        reason.getCloseCode(),
                        reason.getReasonPhrase()));

        getClientsForRoom().remove(this);

        if (this.user != null) {
        	System.out.println(String.format("Removing user [%s] from room [%s]",
        			this.user.getName(), this.roomName));
        	
            getUsersForRoom().remove(this.user);
            
            // todo send updated user list to all clients
        }
    }

    private ByteArrayOutputStream buffer = new ByteArrayOutputStream();
    private User user;

    @OnMessage
    public void onMessage(ByteBuffer byteBuffer, boolean complete, @PathParam("room-name") String roomName) {
        try {
            buffer.write(byteBuffer.array());
            if (complete) {
                FileOutputStream fileOutputStream = null;

                try {
                    File imageFile = File.createTempFile("image", ".jpg");
                    System.out.println(String.format("Saving image to [%s]", imageFile.getAbsolutePath()));
                    fileOutputStream = new FileOutputStream(imageFile);
                    fileOutputStream.write(buffer.toByteArray());
                } finally {
                    if (fileOutputStream != null) {
                        fileOutputStream.flush();
                        fileOutputStream.close();
                    }
                }

                List<ChatEndpoint> roomClients = getClientsForRoom();
                for (ChatEndpoint client : roomClients) {
                    final ByteBuffer sendData = ByteBuffer.allocate(buffer.toByteArray().length);
                    sendData.put(buffer.toByteArray());
                    sendData.rewind();
                    client.session.getAsyncRemote().sendBinary(sendData, new SendHandler() {
                        @Override
                        public void onResult(SendResult sendResult) {
                            System.out.println(
                                    String.format("Async Binary data result for room [%s] : [%s] [%s]",
                                            roomName,
                                            client.session.getId(),
                                            sendResult.isOK())
                            );
                        }
                    });
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    @OnMessage
    public void onMessage(Message message) {
        if (message instanceof JoinMessage) {
            processMessage((JoinMessage) message);
        } else if (message instanceof ChatMessage) {
            processMessage((ChatMessage) message);
        } else if (message instanceof GetUsersMessage) {
            processMessage((GetUsersMessage) message);
        } else {
            System.err.println(String.format("Unknown message type [%s]", message.getClass().getName()));
        }
    }

    private void processMessage(JoinMessage message) {
        User user = new User();
        user.setName(message.getName());
        this.user = user;

        List<User> roomUsers = getUsersForRoom();
        roomUsers.add(user);
        broadcast(message);

        // todo add logging for user added
    }

    private List<User> getUsersForRoom() {
        ChatEndpoint.users.computeIfAbsent(roomName, s -> new CopyOnWriteArrayList<>());
        return users.get(roomName);
    }

    private void processMessage(ChatMessage message) {
        List<ChatMessage> roomMessages = getMessagesForRoom();
        roomMessages.add(message);
        broadcast(message);
    }

    private List<ChatMessage> getMessagesForRoom() {
        ChatEndpoint.messages.computeIfAbsent(roomName, m -> new CopyOnWriteArrayList<>());
        return ChatEndpoint.messages.get(roomName);
    }

    private void processMessage(GetUsersMessage message) {
        try {
            session.getBasicRemote().sendObject(new UserListMessage(getUsersForRoom()));
            session.getBasicRemote().sendObject(new ChatMessagesMessage(getMessagesForRoom()));
        } catch (IOException | EncodeException e) {
            e.printStackTrace();
        }
    }

    private void broadcast(Message message) {
        List<ChatEndpoint> roomClients = getClientsForRoom();

        for (ChatEndpoint client : roomClients) {
            try {
                client.session.getBasicRemote().sendObject(message);
            } catch (IOException e) {
                roomClients.remove(this);
                try {
                    client.session.close();
                } catch (IOException e1) {
                    e1.printStackTrace();
                }
            } catch (EncodeException e) {
                e.printStackTrace();
            }
        }
    }
}
