package net.arunoday.metric.store.repository;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;

import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.arunoday.metric.store.model.ContextData;
import net.arunoday.metric.store.model.GaugeEvent;

import org.apache.commons.lang.math.RandomUtils;
import org.joda.time.DateTime;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.query.Criteria;

/**
 * Tests {@link GaugeEventRepository}
 * 
 * @author Aparna Chaudhary
 */
public class GaugeEventRepositoryIT extends AbstractRepositoryIT {

	private static final String EVENT_TYPE_HOMEREQUEST = "home_response_time";

	@Autowired
	private GaugeEventRepository<String> repository;

	@Before
	public void before() {
		cleanUpDB(GaugeEventRepository.EVENT_COLLECTION);
	}

	@Test
	public void testSave() {

		GaugeEvent event = new GaugeEvent(new Date(), EVENT_TYPE_HOMEREQUEST, RandomUtils.nextDouble());

		ContextData data = new ContextData();
		data.put("request", "localhost");
		event.setContextData(data);
		repository.save(event);

		assertEquals("Expected one event to be stored", 1, repository.count(event.getEventType()));
	}

	@Test
	public void testSaveWithNestedContextData() {

		GaugeEvent event = new GaugeEvent(new Date(), EVENT_TYPE_HOMEREQUEST, RandomUtils.nextDouble());

		ContextData data = new ContextData();
		data.put("request", "localhost");
		Map<String, String> innerData = new HashMap<String, String>();
		innerData.put("ID1", "VALUE1");
		innerData.put("ID2", "VALUE2");
		data.put("data", innerData);

		event.setContextData(data);
		event = repository.save(event);

		assertEquals("Expected one event to be stored", 1, repository.count(event.getEventType()));

		GaugeEvent retrievedEvent = repository.findOne(event.getId(), event.getEventType());
		assertEquals("", retrievedEvent.getContextData(), event.getContextData());
	}

	@Test
	public void testFindOne() {

		GaugeEvent event = new GaugeEvent(new Date(), EVENT_TYPE_HOMEREQUEST, RandomUtils.nextDouble());

		ContextData data = new ContextData();
		data.put("request", "localhost");
		data.put("memory", RandomUtils.nextInt());
		event.setContextData(data);
		event = repository.save(event);

		assertEquals("Expected different event ", event, repository.findOne(event.getId(), EVENT_TYPE_HOMEREQUEST));
		assertNull("Expected no event matching the key", repository.findOne("non-existent", EVENT_TYPE_HOMEREQUEST));
	}

	@Test
	public void testFindWithCriteria() {

		DateTime startDate = new DateTime("2013-08-10T16:00:00.389Z");
		storeEvents(25, startDate, "localhost");
		storeEvents(20, startDate, "someremotemachine");

		List<GaugeEvent> results = (List<GaugeEvent>) repository.find(EVENT_TYPE_HOMEREQUEST,
				Criteria.where("contextData.request").is("localhost"), startDate.toDate(), startDate.plusSeconds(10).toDate(),
				3);
		assertEquals(startDate.toDate(), results.get(0).getOccuredOn());
		assertEquals("", 3, results.size());

		results = (List<GaugeEvent>) repository.find(EVENT_TYPE_HOMEREQUEST, Criteria.where("contextData.index").is(2),
				startDate.toDate(), startDate.plusSeconds(10).toDate(), 3);
		assertEquals(startDate.plusSeconds(2).toDate(), results.get(0).getOccuredOn());
		assertEquals("", 2, results.size());

	}

	@Test
	public void testFindEventTypes() {

		GaugeEvent event1 = new GaugeEvent(new Date(), "request1", RandomUtils.nextDouble());
		GaugeEvent event2 = new GaugeEvent(new Date(), "request2", RandomUtils.nextDouble());

		repository.save(event1);
		repository.save(event2);

		assertEquals("Unexpected event types", Arrays.asList("request1", "request2"), repository.findEventTypes());
	}

	@Test
	public void testDeleteAll() {

		String eventType = "request1";
		repository.save(new GaugeEvent(new DateTime().toDate(), eventType, RandomUtils.nextDouble()));
		repository.save(new GaugeEvent(new DateTime().toDate(), eventType, RandomUtils.nextDouble()));
		assertEquals(2, repository.count(eventType));

		repository.deleteAll(eventType);
		assertEquals(0, repository.count(eventType));
	}

	private void storeEvents(int count, DateTime startDate, String requestType) {
		for (int i = 0; i < count; i++) {
			GaugeEvent event = new GaugeEvent(startDate.plusSeconds(i).toDate(), EVENT_TYPE_HOMEREQUEST,
					RandomUtils.nextDouble());

			ContextData data = new ContextData();
			data.put("request", requestType);
			data.put("index", i);
			event.setContextData(data);
			event = repository.save(event);
		}
	}

}
