package example.event;

import static com.mycila.event.Topic.topic;
import static com.mycila.event.Topics.any;
import static com.mycila.event.Topics.match;
import static com.mycila.event.Topics.not;
import static com.mycila.event.Topics.only;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

@SuppressWarnings("rawtypes")
public final class MatcherTest {

	@Test
	public void test_toString() {
		System.out.println(only("prog/events/a").or(only("b")));
		System.out.println(only("prog/events/a").or(match("prog/events/**")));
	}

	@Test
	public void test_match_topic() {
		assertTrue(only("prog/events/a").matches(topic("prog/events/a")));
		assertTrue(match("prog/events/**").matches(topic("prog/events/a")));
		assertTrue(
				only("prog/events/a").or(only("b")).matches(topic("prog/events/a")));
		assertTrue(only("prog/events/a").or(only("b")).matches(topic("b")));
		assertTrue(any().matches(topic("b")));
		assertTrue(any().matches(topic("")));
		assertTrue(not(match("prog/events/**")).matches(topic("a")));
		assertFalse(not(match("prog/events/**")).matches(topic("prog/events/a")));
	}

	@Test
	public void test_equals() {
		assertEquals(only("prog/events/a").or(only("b")),
				only("prog/events/a").or(only("b")));
		assertEquals(only("prog/events/a").or(match("prog/events/**")),
				only("prog/events/a").or(match("prog/events/**")));
	}

	@Test
	public void test_hashcode() {
		assertEquals(only("prog/events/a").or(only("b")).hashCode(),
				only("prog/events/a").or(only("b")).hashCode());
		assertEquals(only("prog/events/a").or(match("prog/events/**")).hashCode(),
				only("prog/events/a").or(match("prog/events/**")).hashCode());
	}
}