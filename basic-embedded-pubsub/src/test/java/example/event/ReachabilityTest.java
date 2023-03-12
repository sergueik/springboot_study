package example.event;

import com.mycila.event.Event;
import com.mycila.event.Reachability;
import com.mycila.event.Subscriber;
import com.mycila.event.annotation.Reference;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.JUnit4;

import static org.junit.Assert.assertEquals;

@SuppressWarnings("rawtypes")
public final class ReachabilityTest {

	@Test
	public void test() throws Exception {
		assertEquals(Reachability.of(new Object()), Reachability.HARD);
		assertEquals(Reachability.of(new Subscriber() {
			public void onEvent(Event event) throws Exception {
			}
		}), Reachability.HARD);
		assertEquals(Reachability.of(new S()), Reachability.WEAK);
	}

	@Reference(Reachability.WEAK)
	private static class S implements Subscriber {
		public void onEvent(Event event) throws Exception {
		}
	}
}
