package example.event;

import static com.mycila.event.Reachability.WEAK;
import static org.junit.Assert.assertEquals;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;

import com.mycila.event.Dispatcher;
import com.mycila.event.Dispatchers;
import com.mycila.event.ErrorHandlers;
import com.mycila.event.Event;
import com.mycila.event.MycilaEvent;
import com.mycila.event.annotation.Group;
import com.mycila.event.annotation.Publish;
import com.mycila.event.annotation.Reference;
import com.mycila.event.annotation.Subscribe;


public final class AnnotationProcessorTest {

	private final List<Object> sequence = new ArrayList<>();

	MycilaEvent processor;
	Dispatcher dispatcher;

	@Before
	public void setup() {
		dispatcher = Dispatchers.synchronousUnsafe(ErrorHandlers.rethrow());
		processor = MycilaEvent.with(dispatcher);
		sequence.clear();
	}

	@Test
	public void test_subscribe_strong() {
		Object o = new Object() {
			@Subscribe(topics = { "prog/events/a",
					"prog/events/**" }, eventType = String.class)
			private void handle(Event<String> event) {
				sequence.add(event.getSource());
			}
		};
		processor.register(o);
		publish();
		assertEquals(sequence.toString(),
				"[Hello for a, Hello for a1, Hello for a1]");
	}

	@Test
	public void test_subscribe_weak() {
		Object o = new Object() {
			@Subscribe(topics = { "prog/events/a",
					"prog/events/b/**" }, eventType = String.class)
			@Reference(WEAK)
			private void handle(Event<String> event) {
				sequence.add(event.getSource());
			}
		};
		processor.register(o);

		System.gc();
		System.gc();
		System.gc();

		publish();
		assertEquals(sequence.toString(), "[]");
	}

	@Test
	public void test_multiple_publish() {
		Object o = new Object() {
			@Subscribe(topics = "prog/events/a/**", eventType = String.class)
			private void handle(Event<String> event) {
				sequence.add(event.getSource());
			}
		};
		processor.register(o);
		publish();
		assertEquals(sequence.toString(),
				"[Hello for a, Hello for a1, Hello for a1]");
	}

	@Test
	public void test_group() {
		final List<Object> events = new ArrayList<Object>();
		Object o = new Object() {
			@Subscribe(topics = "prog/events/group1")
			private void handle1(String a, int b) {
				events.add("handle1-" + a + b);
			}

			@Subscribe(topics = "prog/events/group2")
			private void handle2(String a, int b) {
				events.add("handle2-" + a + b);
			}

			@Subscribe(topics = "prog/events/group3")
			private void handle3(String a) {
				events.add("handle3-" + a);
			}
		};
		processor.register(o);
		C c = processor.instanciate(C.class);
		c.send2("hello", 3);
		c.send3("bonjour");
		assertEquals(events.toString(),
				"[handle1-hello3, handle2-hello3, handle3-bonjour]");
	}

	private void publish() {
		B b = processor.instanciate(B.class);
		C c = processor.instanciate(C.class);
		b.send("Hello for a", 1);
		c.send("Hello for a1", 4);
	}

	private static interface B {
		@Publish(topics = "prog/events/a")
		void send(String a, int b);
	}

	static abstract class C {
		@Publish(topics = { "prog/events/a/a1", "prog/events/a/allA" })
		abstract void send(String a, int b);

		@Publish(topics = { "prog/events/group1", "prog/events/group2" })
		@Group
		abstract void send2(String a, int b);

		@Publish(topics = { "prog/events/group3" })
		@Group
		abstract void send3(String a);
	}
}
