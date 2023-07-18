package example.exception;

import example.model.NotificationTopic;

public class NotificationDeserializationException extends RuntimeException {

    public NotificationDeserializationException(NotificationTopic topic, Throwable cause) {
        super(String.format("Cannot deserialize the notification for topic [%s]", topic), cause);
    }

}
