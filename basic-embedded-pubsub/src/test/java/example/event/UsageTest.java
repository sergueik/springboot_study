package example.event;

import org.junit.Ignore;

import com.mycila.event.Dispatcher;
import com.mycila.event.Dispatchers;
import com.mycila.event.ErrorHandlers;
import com.mycila.event.Event;
import com.mycila.event.Subscriber;
import com.mycila.event.Topics;

import static com.mycila.event.Topic.match;
import static com.mycila.event.Topic.only;
import static com.mycila.event.Topic.topic;

@Ignore
final class UsageTest {
	public static void main(String... args) {
		// first createPublisher an event service
		Dispatcher dispatcher = Dispatchers
				.synchronousUnsafe(ErrorHandlers.rethrow());

		// then subscribe
		Topics topics = only("app/events/swing/button")
				.or(match("app/events/swing/fields/**"));
		dispatcher.subscribe(topics, String.class, new Subscriber<String>() {
			public void onEvent(Event<String> event) throws Exception {
				System.out.println("Received: " + event.getSource());
			}
		});

		// and publish
		dispatcher.publish(topic("app/events/swing/button"), "Hello !");
	}
}
