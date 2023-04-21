package net.arunoday.metric.store.repository;

import static net.arunoday.metric.store.model.MetricResolution.DAY;
import static net.arunoday.metric.store.model.MetricResolution.HOUR;
import static net.arunoday.metric.store.model.MetricResolution.MINUTE;
import static net.arunoday.metric.store.model.MetricResolution.MONTH;
import static net.arunoday.metric.store.model.MetricResolution.YEAR;
import static org.junit.Assert.assertEquals;

import java.util.List;

import net.arunoday.metric.store.model.GaugeEvent;
import net.arunoday.metric.store.model.HierarchialAggregationResult;

import org.apache.commons.lang.math.RandomUtils;
import org.joda.time.DateTime;
import org.junit.Before;
import org.junit.Test;
import org.springframework.beans.factory.annotation.Autowired;

/**
 * Tests {@link GaugeMetricRepository}
 * 
 * @author Aparna Chaudhary
 */
public class GaugeMetricRepositoryIT extends AbstractRepositoryIT {

	private static final String EVENT_TYPE = "test";

	@Autowired
	private GaugeMetricRepository<String> metricRepository;
	@Autowired
	private GaugeEventRepository<String> eventRepository;

	@Before
	public void before() {
		cleanUpDB(GaugeEventRepository.EVENT_COLLECTION);
	}

	@Test
	public void testMinuteWiseAggregation() {
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(MINUTE.getCode()));
		DateTime now = new DateTime();
		DateTime ts = new DateTime(2013, 8, 10, 16, 01, 30);

		storeEvents(10, ts, EVENT_TYPE);
		metricRepository.aggregatePerMinute(EVENT_TYPE, ts.toDate(), now.toDate());
		List<HierarchialAggregationResult> result = (List<HierarchialAggregationResult>) metricRepository.find(EVENT_TYPE,
				MINUTE, ts.minusMinutes(5).toDate(), now.toDate());

		assertEquals("Invalid aggregation result ", 1, result.size());
		assertEquals(10, (Double) result.get(0).get("count"), 0.00);

		storeEvents(2, ts.plusSeconds(40), EVENT_TYPE);
		metricRepository.aggregatePerMinute(EVENT_TYPE, ts.toDate(), now.toDate());
		result = (List<HierarchialAggregationResult>) metricRepository.find(EVENT_TYPE, MINUTE,
				ts
				.minusMinutes(5).toDate(), now.toDate());

		assertEquals("Invalid aggregation result ", 2, result.size());
		assertEquals(10, (Double) result.get(0).get("count"), 0.00);
		assertEquals(2, (Double) result.get(1).get("count"), 0.00);
	}

	@Test
	public void testHourlyAggregation() {
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(MINUTE.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(HOUR.getCode()));

		DateTime now = new DateTime();
		DateTime ts = new DateTime(2013, 8, 10, 16, 01, 30);

		// given
		storeEvents(10, ts, EVENT_TYPE);
		storeEvents(5, ts.plusMinutes(40), EVENT_TYPE);
		storeEvents(4, ts.plusHours(1), EVENT_TYPE);
		metricRepository.aggregatePerMinute(EVENT_TYPE, ts.toDate(), now.toDate());

		// when
		metricRepository.aggregatePerHour(EVENT_TYPE, new DateTime(2013, 8, 10, 16, 00, 00).toDate(), now.toDate());
		List<HierarchialAggregationResult> result = (List<HierarchialAggregationResult>) metricRepository.find(EVENT_TYPE,
				HOUR, new DateTime(2013, 8, 10, 16, 00, 00).minusMinutes(5).toDate(), now.toDate());

		// then
		assertEquals("Invalid aggregation result ", 2, result.size());
		assertEquals(15, (Double) result.get(0).get("count"), 0.00);
		assertEquals(4, (Double) result.get(1).get("count"), 0.00);
	}

	@Test
	public void testDailyAggregation() {
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(MINUTE.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(HOUR.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(DAY.getCode()));

		DateTime now = new DateTime();
		DateTime ts = new DateTime(2013, 8, 10, 16, 01, 30);

		// given
		storeEvents(10, ts, EVENT_TYPE);
		storeEvents(5, ts.plusMinutes(40), EVENT_TYPE);
		storeEvents(4, ts.plusHours(1), EVENT_TYPE);
		storeEvents(3, ts.plusHours(25), EVENT_TYPE);
		metricRepository.aggregatePerMinute(EVENT_TYPE, ts.toDate(), now.toDate());
		metricRepository.aggregatePerHour(EVENT_TYPE, new DateTime(2013, 8, 10, 16, 00, 00).toDate(), now.toDate());

		// when
		metricRepository.aggregatePerDay(EVENT_TYPE, new DateTime(2013, 8, 10, 00, 00, 00).toDate(), now.toDate());
		List<HierarchialAggregationResult> result = (List<HierarchialAggregationResult>) metricRepository.find(EVENT_TYPE,
				DAY, new DateTime(2013, 8, 10, 00, 00, 00).toDate(), now.toDate());

		// then
		assertEquals("Invalid aggregation result ", 2, result.size());
		assertEquals(19, (Double) result.get(0).get("count"), 0.00);
		assertEquals(3, (Double) result.get(1).get("count"), 0.00);
	}

	@Test
	public void testMonthlyAggregation() {
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(MINUTE.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(HOUR.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(DAY.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(MONTH.getCode()));

		DateTime now = new DateTime();
		DateTime ts = new DateTime(2013, 8, 10, 16, 01, 30);

		// given
		storeEvents(10, ts, EVENT_TYPE);
		storeEvents(5, ts.plusMinutes(40), EVENT_TYPE);
		storeEvents(4, ts.plusHours(1), EVENT_TYPE);
		storeEvents(3, ts.plusHours(25), EVENT_TYPE);
		storeEvents(7, ts.plusDays(34), EVENT_TYPE);
		metricRepository.aggregatePerMinute(EVENT_TYPE, ts.toDate(), now.toDate());
		metricRepository.aggregatePerHour(EVENT_TYPE, new DateTime(2013, 8, 10, 16, 00, 00).toDate(), now.toDate());
		metricRepository.aggregatePerDay(EVENT_TYPE, new DateTime(2013, 8, 10, 00, 00, 00).toDate(), now.toDate());

		// when
		metricRepository.aggregatePerMonth(EVENT_TYPE, new DateTime(2013, 8, 01, 00, 00, 00).toDate(), now.toDate());
		List<HierarchialAggregationResult> result = (List<HierarchialAggregationResult>) metricRepository.find(EVENT_TYPE,
				MONTH, new DateTime(2013, 07, 31, 00, 00, 00).toDate(), now.toDate());

		// then
		assertEquals("Invalid aggregation result ", 2, result.size());
		assertEquals(22, (Double) result.get(0).get("count"), 0.00);
		assertEquals(7, (Double) result.get(1).get("count"), 0.00);
	}

	@Test
	public void testYearlyAggregation() {
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(MINUTE.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(HOUR.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(DAY.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(MONTH.getCode()));
		mongoOperations.dropCollection(EVENT_TYPE.concat(".").concat(YEAR.getCode()));

		DateTime now = new DateTime();
		DateTime ts = new DateTime(2013, 8, 10, 16, 01, 30);

		// given
		storeEvents(10, ts, EVENT_TYPE);
		storeEvents(5, ts.plusMinutes(40), EVENT_TYPE);
		storeEvents(4, ts.plusHours(1), EVENT_TYPE);
		storeEvents(3, ts.plusHours(25), EVENT_TYPE);
		storeEvents(7, ts.plusDays(34), EVENT_TYPE);
		metricRepository.aggregatePerMinute(EVENT_TYPE, ts.toDate(), now.toDate());
		metricRepository.aggregatePerHour(EVENT_TYPE, new DateTime(2013, 8, 10, 16, 00, 00).toDate(), now.toDate());
		metricRepository.aggregatePerDay(EVENT_TYPE, new DateTime(2013, 8, 10, 00, 00, 00).toDate(), now.toDate());
		metricRepository.aggregatePerMonth(EVENT_TYPE, new DateTime(2013, 8, 01, 00, 00, 00).toDate(), now.toDate());

		// when
		metricRepository.aggregatePerYear(EVENT_TYPE, new DateTime(2012, 01, 01, 00, 00, 00).toDate(), now.toDate());
		List<HierarchialAggregationResult> result = (List<HierarchialAggregationResult>) metricRepository.find(EVENT_TYPE,
				YEAR, new DateTime(2012, 01, 01, 00, 00, 00).toDate(), now.toDate());

		// then
		assertEquals("Invalid aggregation result ", 1, result.size());
		assertEquals(29, (Double) result.get(0).get("count"), 0.00);
	}

	private void storeEvents(int count, DateTime startDate, String requestType) {
		for (int i = 0; i < count; i++) {
			GaugeEvent event = new GaugeEvent(startDate.plusSeconds(i).toDate(), requestType,
					RandomUtils.nextDouble());
			event = eventRepository.save(event);
		}
	}

}
