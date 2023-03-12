package example.event;

import static com.mycila.event.Topic.topic;
import static com.mycila.event.Topics.match;
import static com.mycila.event.Topics.only;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.mycila.event.Dispatcher;
import com.mycila.event.Dispatchers;
import com.mycila.event.ErrorHandlers;
import com.mycila.event.Event;
import com.mycila.event.Reachability;
import com.mycila.event.Subscriber;
import com.mycila.event.Topic;
import com.mycila.event.annotation.Reference;

@SuppressWarnings("rawtypes")
public final class DefaultDispatcherTest {

	private final List<Object> sequence = new ArrayList<>();

	private Dispatcher dispatcher;

	@Before
	public void setup() {
		dispatcher = Dispatchers.synchronousUnsafe(ErrorHandlers.rethrow());
		sequence.clear();
	}

	@After
	public void close() {
		dispatcher.close();
	}

	@Test
	public void test_subscribe_strong() {
		dispatcher.subscribe(only("prog/events/a").or(match("prog/events/b/**")),
				String.class, new Subscriber<String>() {
					public void onEvent(Event<String> event) throws Exception {
						sequence.add(event.getSource());
					}
				});
		publish();
		assertEquals(sequence.toString(),
				"[Hello for a, hello for b1, hello for b2]");
	}

	@Test
	public void test_subscribe_weak() {
		@Reference(Reachability.WEAK)
		class C implements Subscriber<String> {
			public void onEvent(Event<String> event) throws Exception {
				sequence.add(event.getSource());
			}
		}
		dispatcher.subscribe(only("prog/events/a").or(match("prog/events/b/**")),
				String.class, new C());

		System.gc();
		System.gc();
		System.gc();

		publish();
		assertEquals(sequence.toString(), "[]");
	}

	@Test
	public void test_SYNCHRONOUS_SAFE_DISPCHER() throws InterruptedException {
		final Dispatcher dispatcher = Dispatchers
				.synchronousSafe(ErrorHandlers.rethrow());

		final CountDownLatch go = new CountDownLatch(1);
		final CountDownLatch finished = new CountDownLatch(20);

		for (int i = 0; i < 20; i++) {
			new Thread("T" + i) {
				@Override
				public void run() {
					try {
						go.await();
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
					dispatcher.publish(Topic.topic("a/b"),
							Thread.currentThread().getName());
					finished.countDown();
				}
			}.start();
		}

		final AtomicBoolean inProcess = new AtomicBoolean(false);

		for (int i = 0; i < 20; i++) {
			final int index = i;
			dispatcher.subscribe(Topic.only("a/b"), String.class,
					new Subscriber<String>() {

						public void onEvent(Event<String> event) throws Exception {
							if (inProcess.get())
								fail();
							inProcess.set(true);
							Thread.sleep(50);
							// System.out.println("[" + Thread.currentThread().getName() + "]
							// " + " S" + index + " - " + event.getSource());
							inProcess.set(false);
						}
					});
		}

		go.countDown();
		finished.await();
		dispatcher.close();
	}

	@Test
	public void test_SYNCHRONOUS_UNSAFE_DISPATCHER() throws InterruptedException {
		final Dispatcher dispatcher = Dispatchers
				.synchronousUnsafe(ErrorHandlers.rethrow());

		final CountDownLatch go = new CountDownLatch(1);
		final CountDownLatch finished = new CountDownLatch(20);

		for (int i = 0; i < 20; i++) {
			new Thread("T" + i) {

				public void run() {
					try {
						go.await();
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
					dispatcher.publish(Topic.topic("a/b"),
							Thread.currentThread().getName());
					finished.countDown();
				}
			}.start();
		}

		final AtomicBoolean inProcess = new AtomicBoolean(false);
		final AtomicInteger paralellCalls = new AtomicInteger(0);

		for (int i = 0; i < 20; i++) {
			final int index = i;
			dispatcher.subscribe(Topic.only("a/b"), String.class,
					new Subscriber<String>() {

						public void onEvent(Event<String> event) throws Exception {
							if (inProcess.get())
								paralellCalls.incrementAndGet();
							inProcess.set(true);
							Thread.sleep(200);
							// System.out.println("[" + Thread.currentThread().getName() + "]
							// " + " S" + index + " - " + event.getSource());
							inProcess.set(false);
						}
					});
		}

		go.countDown();
		finished.await();

		System.out.println("paralellCalls.get()=" + paralellCalls.get());
		assertTrue(paralellCalls.get() > 10);
		dispatcher.close();
	}

	@Test
	public void test_ASYNCHRONOUS_SAFE_DISPATCHER() throws InterruptedException {
		final Dispatcher dispatcher = Dispatchers
				.asynchronousSafe(ErrorHandlers.rethrow());

		final CountDownLatch go = new CountDownLatch(1);
		final CountDownLatch publish = new CountDownLatch(20);
		final CountDownLatch consume = new CountDownLatch(20 * 20);

		for (int i = 0; i < 20; i++) {
			new Thread("T" + i) {

				public void run() {
					try {
						go.await();
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
					dispatcher.publish(Topic.topic("a/b"),
							Thread.currentThread().getName());
					publish.countDown();
				}
			}.start();
		}

		final AtomicBoolean inProcess = new AtomicBoolean(false);
		final AtomicInteger paralellCalls = new AtomicInteger(0);

		for (int i = 0; i < 20; i++) {
			final int index = i;
			dispatcher.subscribe(Topic.only("a/b"), String.class,
					new Subscriber<String>() {

						public void onEvent(Event<String> event) throws Exception {
							if (inProcess.get())
								paralellCalls.incrementAndGet();
							inProcess.set(true);
							Thread.sleep(10);
							// System.out.println("[" + Thread.currentThread().getName() + "]
							// " + " S" + index + " - " + event.getSource());
							inProcess.set(false);
							consume.countDown();
						}
					});
		}

		go.countDown();
		publish.await();

		System.out.println("publishing finished, waiting for consumers...");

		consume.await();

		System.out.println("paralellCalls.get()=" + paralellCalls.get());
		assertEquals(paralellCalls.get(), 0);
		dispatcher.close();
	}

	@Test
	public void test_ASYNCHRONOUS_UNSAFE_DISPATCHER()
			throws InterruptedException {
		final Dispatcher dispatcher = Dispatchers.asynchronousUnsafe(30,
				ErrorHandlers.rethrow());

		final CountDownLatch go = new CountDownLatch(1);
		final CountDownLatch publish = new CountDownLatch(20);
		final CountDownLatch consume = new CountDownLatch(20 * 20);

		for (int i = 0; i < 20; i++) {
			new Thread("T" + i) {

				public void run() {
					try {
						go.await();
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
					dispatcher.publish(Topic.topic("a/b"),
							Thread.currentThread().getName());
					publish.countDown();
				}
			}.start();
		}

		final AtomicBoolean inProcess = new AtomicBoolean(false);
		final AtomicInteger paralellCalls = new AtomicInteger(0);

		for (int i = 0; i < 20; i++) {
			final int index = i;
			dispatcher.subscribe(Topic.only("a/b"), String.class,
					new Subscriber<String>() {

						public void onEvent(Event<String> event) throws Exception {
							if (inProcess.get())
								paralellCalls.incrementAndGet();
							inProcess.set(true);
							Thread.sleep(100);
							// System.out.println("[" + Thread.currentThread().getName() + "]
							// " + " S" + index + " - " + event.getSource());
							inProcess.set(false);
							consume.countDown();
						}
					});
		}

		go.countDown();
		publish.await();

		System.out.println("publishing finished, waiting for consumers...");

		consume.await();

		System.out.println("paralellCalls.get()=" + paralellCalls.get());
		assertTrue(paralellCalls.get() > 5);
		dispatcher.close();
	}

	@Test
	public void test_BROADCAST_ORDERED_DISPATCHER() throws InterruptedException {
		final Dispatcher dispatcher = Dispatchers.broadcastOrdered(30,
				ErrorHandlers.rethrow());

		final CountDownLatch go = new CountDownLatch(1);
		final CountDownLatch publish = new CountDownLatch(20);
		final CountDownLatch consume = new CountDownLatch(20 * 20);

		for (int i = 0; i < 20; i++) {
			new Thread("T" + i) {

				public void run() {
					try {
						go.await();
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
					dispatcher.publish(Topic.topic("a/b"),
							Thread.currentThread().getName());
					publish.countDown();
				}
			}.start();
		}

		final AtomicBoolean inProcess = new AtomicBoolean(false);
		final AtomicInteger paralellCalls = new AtomicInteger(0);

		for (int i = 0; i < 20; i++) {
			final int index = i;
			dispatcher.subscribe(Topic.only("a/b"), String.class,
					new Subscriber<String>() {

						public void onEvent(Event<String> event) throws Exception {
							if (inProcess.get())
								paralellCalls.incrementAndGet();
							inProcess.set(true);
							Thread.sleep(100);
							// System.out.println("[" + Thread.currentThread().getName() + "]
							// " + " S" + index + " - " + event.getSource());
							inProcess.set(false);
							consume.countDown();
						}
					});
		}

		go.countDown();
		publish.await();

		System.out.println("publishing finished, waiting for consumers...");

		consume.await();

		System.out.println("paralellCalls.get()=" + paralellCalls.get());
		assertTrue(paralellCalls.get() > 30);
		dispatcher.close();
	}

	@Test
	public void test_BROADCAST_UNORDERED_DISPATCHER()
			throws InterruptedException {
		final Dispatcher dispatcher = Dispatchers.broadcastUnordered(30,
				ErrorHandlers.rethrow());

		final CountDownLatch go = new CountDownLatch(1);
		final CountDownLatch publish = new CountDownLatch(20);
		final CountDownLatch consume = new CountDownLatch(20 * 20);

		for (int i = 0; i < 20; i++) {
			new Thread("T" + i) {

				public void run() {
					try {
						go.await();
					} catch (InterruptedException e) {
						Thread.currentThread().interrupt();
					}
					dispatcher.publish(Topic.topic("a/b"),
							Thread.currentThread().getName());
					publish.countDown();
				}
			}.start();
		}

		final AtomicBoolean inProcess = new AtomicBoolean(false);
		final AtomicInteger paralellCalls = new AtomicInteger(0);

		for (int i = 0; i < 20; i++) {
			final int index = i;
			dispatcher.subscribe(Topic.only("a/b"), String.class,
					new Subscriber<String>() {

						public void onEvent(Event<String> event) throws Exception {
							if (inProcess.get())
								paralellCalls.incrementAndGet();
							inProcess.set(true);
							Thread.sleep(100);
							// System.out.println("[" + Thread.currentThread().getName() + "]
							// " + " S" + index + " - " + event.getSource());
							inProcess.set(false);
							consume.countDown();
						}
					});
		}

		go.countDown();
		publish.await();

		System.out.println("publishing finished, waiting for consumers...");

		consume.await();

		System.out.println("paralellCalls.get()=" + paralellCalls.get());
		assertTrue(paralellCalls.get() > 5);
		dispatcher.close();
	}

	private void publish() {
		dispatcher.publish(topic("prog/events/a"), "Hello for a");
		dispatcher.publish(topic("prog/events/a"), 1);

		dispatcher.publish(topic("prog/events/b/b1"), "hello for b1");
		dispatcher.publish(topic("prog/events/b/b1"), 2);

		dispatcher.publish(topic("prog/events/b/b1"), "hello for b2");
		dispatcher.publish(topic("prog/events/b/b1"), 3);

		dispatcher.publish(topic("prog/events/a/a1"), "hello for a1");
		dispatcher.publish(topic("prog/events/a/a1"), 4);
	}
}
