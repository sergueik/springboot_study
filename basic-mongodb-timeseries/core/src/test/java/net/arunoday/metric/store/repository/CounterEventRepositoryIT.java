/**
 * 
 */
package net.arunoday.metric.store.repository;

import static net.arunoday.metric.store.model.MetricOperation.MAX;
import static net.arunoday.metric.store.model.MetricOperation.MIN;
import static org.junit.Assert.assertEquals;

import java.util.Date;

import net.arunoday.metric.store.model.CounterEventEntity;

import org.joda.time.DateTime;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests {@link CounterEventRepository}
 * 
 * @author Aparna Chaudhary
 */
public class CounterEventRepositoryIT extends AbstractRepositoryIT {

	@Autowired
	private CounterEventRepository<String> repository;

	@Before
	public void before() {
		repository.deleteAll();
	}

	@Test
	public void testFindById() {
		CounterEventEntity event1 = new CounterEventEntity();
		event1.setOccuredOn(new Date());
		event1.setEventType("request-home");
		event1.setTotalCount(2345L);
		event1 = repository.save(event1, false);

		CounterEventEntity event2 = new CounterEventEntity();
		event2.setOccuredOn(new Date());
		event2.setEventType("request-home");
		event2.setTotalCount(1234L);
		event2 = repository.save(event2, false);

		CounterEventEntity resultEvent = repository.findOne(event1.getId(), "request-home");
		assertEquals(event1, resultEvent);

	}

	@Test
	public void testDelete() {
		CounterEventEntity event;
		for (long i = 0; i < 100; i++) {
			event = new CounterEventEntity();
			event.setOccuredOn(new Date());
			event.setEventType("request-home");
			event.setTotalCount(i);
			event = repository.save(event, false);
		}
		assertEquals(100, repository.count("request-home"));
		repository.deleteAll("request-home");
		assertEquals(0, repository.count("request-home"));
	}

	/**
	 * Tests the minimum value of count is returned correctly.
	 */
	@Test
	public void testMinRequestCount() {
		DateTime startDate = new DateTime("2013-08-10T16:00:00.389Z");
		long initialTotal = 10;
		CounterEventEntity event;
		for (int i = 0; i < 100; i++) {
			event = new CounterEventEntity();
			event.setOccuredOn(startDate.plusSeconds(i).toDate());
			event.setEventType("request-home");
			event.setTotalCount(initialTotal + i);
			event = repository.save(event, false);
		}
		for (long i = 0; i < 10; i++) {
			event = new CounterEventEntity();
			event.setOccuredOn(new Date());
			event.setEventType("request2-home");
			event.setTotalCount(i);
			event = repository.save(event, false);
		}
		assertEquals(10, repository.performAggregation("request-home", MIN, null, null), 0.00);
		assertEquals(10, repository.performAggregation("request-home", MIN, startDate.toDate(), null), 0.00);
		assertEquals(10, repository.performAggregation("request-home", MIN, null, startDate.plusSeconds(15).toDate()), 0.00);
		assertEquals(10,
				repository.performAggregation("request-home", MIN, startDate.toDate(), startDate.plusSeconds(15).toDate()),
				0.00);
		assertEquals(0, repository.performAggregation("request2-home", MIN, null, null), 0.00);
	}

	/**
	 * Tests increment and decrement of counter values
	 */
	@Test
	public void testIncrementAndDecrement() {
		CounterEventEntity event = new CounterEventEntity();
		DateTime startDate = new DateTime("2013-08-10T16:00:00.389Z");
		event.setOccuredOn(startDate.toDate());
		event.setEventType("request-home");
		event.setTotalCount(10L);
		event = repository.save(event, false);
		assertEquals(10, repository.performAggregation("request-home", MIN, null, null), 0.00);

		// increment counter
		CounterEventEntity incrementCounter = new CounterEventEntity();
		incrementCounter.setOccuredOn(new Date());
		incrementCounter.setEventType("request-home");
		incrementCounter.setTotalCount(10L);
		incrementCounter = repository.save(incrementCounter, false);
		assertEquals(10, repository.performAggregation("request-home", MIN, null, null), 0.00);
		assertEquals(20, repository.performAggregation("request-home", MAX, null, null), 0.00);

		// decrement counter
		CounterEventEntity decrementCounter = new CounterEventEntity();
		decrementCounter.setOccuredOn(new Date());
		decrementCounter.setEventType("request-home");
		decrementCounter.setTotalCount(5L);
		decrementCounter = repository.save(decrementCounter, true);
		assertEquals(15, decrementCounter.getTotalCount(), 0.00);
		assertEquals(10, repository.performAggregation("request-home", MIN, null, null), 0.00);
		assertEquals(20, repository.performAggregation("request-home", MAX, null, null), 0.00);
	}

}
