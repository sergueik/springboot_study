package example.service;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.PropertyNamingStrategy;
import com.fasterxml.jackson.datatype.jsr310.JavaTimeModule;
import example.exception.NotificationDeserializationException;
import example.model.NotificationTopic;
import io.micrometer.core.instrument.util.StringUtils;
import io.r2dbc.postgresql.api.PostgresqlConnection;
import io.r2dbc.spi.ConnectionFactory;
import org.springframework.stereotype.Service;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;
import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import java.util.HashSet;
import java.util.Set;

@Service
public class NotificationService {
	@java.lang.SuppressWarnings("all")
	private static final org.slf4j.Logger log = org.slf4j.LoggerFactory
			.getLogger(NotificationService.class);
	private final ConnectionFactory connectionFactory;
	private final Set<NotificationTopic> watchedTopics = new HashSet<>();
	private PostgresqlConnection connection;
	private ObjectMapper objectMapper;

	/**
	 * Listen to a postgreSQL topic
	 *
	 * @param topic Topic to which the connection needs to subscribe
	 * @param clazz class of the notification parameter (used for deserialization)
	 * @return the notification parameters
	 */
	public <T> Flux<T> listen(final NotificationTopic topic,
			final Class<T> clazz) {
		// Listen to the topic
		if (!watchedTopics.contains(topic)) {
			synchronized (watchedTopics) {
				if (!watchedTopics.contains(topic)) {
					executeListenStatement(topic);
					watchedTopics.add(topic);
				}
			}
		}
		// Get the notifications
		return getConnection().getNotifications()
				.filter(notification -> topic.name().equals(notification.getName())
						&& notification.getParameter() != null)
				.handle((notification, sink) -> {
					final String json = notification.getParameter();
					if (!StringUtils.isBlank(json)) {
						try {
							sink.next(objectMapper.readValue(json, clazz));
						} catch (JsonProcessingException e) {
							log.error(String.format(
									"Problem deserializing an instance of [%s] with the following json: %s ",
									clazz.getSimpleName(), json), e);
							Mono.error(new NotificationDeserializationException(topic, e));
						}
					}
				});
	}

	/**
	 * Unlisten from a postgreSQL topic
	 *
	 * @param topic Topic to which the connection needs to unsubscribe
	 */
	public void unlisten(final NotificationTopic topic) {
		if (watchedTopics.contains(topic)) {
			synchronized (watchedTopics) {
				if (watchedTopics.contains(topic)) {
					executeUnlistenStatement(topic);
					watchedTopics.remove(topic);
				}
			}
		}
	}

	@PostConstruct
	private void postConstruct() {
		this.objectMapper = createObjectMapper();
	}

	@PreDestroy
	private void preDestroy() {
		this.getConnection().close().subscribe();
	}

	/**
	 * Execute the SQL statement used to listen to a given topic
	 *
	 * @param topic Name of the topic to listen to
	 */
	private void executeListenStatement(final NotificationTopic topic) {
		// Topic in upper-case must be surrounded by quotes
		getConnection().createStatement(String.format("LISTEN \"%s\"", topic))
				.execute().subscribe();
	}

	/**
	 * Execute the SQL statement used to unlisten from a given topic
	 *
	 * @param topic Name of the topic to unlisten from
	 */
	private void executeUnlistenStatement(final NotificationTopic topic) {
		// Topic in upper-case must be surrounded by quotes
		getConnection().createStatement(String.format("UNLISTEN \"%s\"", topic))
				.execute().subscribe();
	}

	/**
	 * Get or create a PostgreSQL database connection
	 *
	 * @return the connection created synchronously
	 */
	private PostgresqlConnection getConnection() {
		if (connection == null) {
			synchronized (NotificationService.class) {
				if (connection == null) {
					connection = Mono.from(connectionFactory.create())
							.cast(PostgresqlConnection.class).block();
				}
			}
		}
		return this.connection;
	}

	/**
	 * Create an object mapper to convert the json notification
	 * parameters to entities
	 *
	 * @return the object mapper
	 */
	private ObjectMapper createObjectMapper() {
		return
		// This strategy is needed to match the DB column names with the entity
		// field names
		// Ignore the missing properties
		new ObjectMapper().registerModule(new JavaTimeModule())
				.setPropertyNamingStrategy(PropertyNamingStrategy.SNAKE_CASE)
				.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES, false);
	}

	@java.lang.SuppressWarnings("all")
	public NotificationService(final ConnectionFactory connectionFactory) {
		this.connectionFactory = connectionFactory;
	}
}
