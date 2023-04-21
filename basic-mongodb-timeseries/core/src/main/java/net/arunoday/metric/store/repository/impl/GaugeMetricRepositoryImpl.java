package net.arunoday.metric.store.repository.impl;

import static net.arunoday.metric.store.model.GaugeEvent.EVENT_TYPE_FIELD;
import static net.arunoday.metric.store.model.GaugeEvent.OCCURED_ON_FIELD;
import static net.arunoday.metric.store.model.MetricResolution.DAY;
import static net.arunoday.metric.store.model.MetricResolution.HOUR;
import static net.arunoday.metric.store.model.MetricResolution.MINUTE;
import static net.arunoday.metric.store.model.MetricResolution.MONTH;
import static net.arunoday.metric.store.model.MetricResolution.YEAR;

import java.util.Date;
import java.util.List;

import net.arunoday.metric.store.model.HierarchialAggregationResult;
import net.arunoday.metric.store.model.MetricResolution;
import net.arunoday.metric.store.repository.GaugeEventRepository;
import net.arunoday.metric.store.repository.GaugeMetricRepository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.mapreduce.MapReduceOptions;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

/**
 * Default MongoDB implementation for {@link GaugeMetricRepository}
 * 
 * @author Aparna Chaudhary
 */
@Repository
public class GaugeMetricRepositoryImpl implements GaugeMetricRepository<String> {

	private static final Logger logger = LoggerFactory.getLogger(GaugeMetricRepositoryImpl.class);

	@Autowired
	@Qualifier("metricMongoTemplate")
	private MongoTemplate metricMongoTemplate;

	@Autowired
	@Qualifier("eventMongoTemplate")
	private MongoTemplate eventMongoTemplate;

	@Override
	public Iterable<HierarchialAggregationResult> find(String eventName, MetricResolution resolution, Date startTime,
			Date endTime) {
		Query query = new Query().addCriteria(new Criteria("_id").gte(startTime).lt(endTime));
		logger.debug(String.format("Criteria for event [%s] aggregation query %s ", eventName, query));
		List<HierarchialAggregationResult> result = metricMongoTemplate.find(query, HierarchialAggregationResult.class,
				getCollection(eventName, resolution));
		return result;
	}

	@Override
	public void aggregatePerMinute(String eventName, Date startDate, Date endDate) {
		long lStartTime = System.currentTimeMillis();
		Criteria criteria = Criteria.where(EVENT_TYPE_FIELD).is(eventName);
		criteria = prepareDateCriteria(criteria, OCCURED_ON_FIELD, startDate, endDate);
		eventMongoTemplate.mapReduce(
				new Query(criteria),
				getEventCollectionName(eventName),
				"classpath:minute_map_function.js",
				"classpath:reduce_function.js",
				new MapReduceOptions().outputDatabase(metricMongoTemplate.getDb().getName())
						.outputCollection(getCollection(eventName, MINUTE)).outputTypeMerge()
						.finalizeFunction("classpath:finalize_function.js"), HierarchialAggregationResult.class);

		long lEndTime = System.currentTimeMillis();
		logger.debug(String.format("Total time aggregatePerMinute(): %s msec ", (lEndTime - lStartTime)));
	}

	@Override
	public void aggregatePerHour(String eventName, Date startDate, Date endDate) {
		long lStartTime = System.currentTimeMillis();

		Criteria criteria = new Criteria();
		criteria = prepareDateCriteria(criteria, "_id", startDate, endDate);
		metricMongoTemplate.mapReduce(
				new Query(criteria),
				getCollection(eventName, MINUTE),
				"classpath:hourly_map_function.js",
				"classpath:reduce_function.js",
				new MapReduceOptions().outputCollection(getCollection(eventName, HOUR)).outputTypeMerge()
						.finalizeFunction("classpath:finalize_function.js"), HierarchialAggregationResult.class);

		long lEndTime = System.currentTimeMillis();
		logger.debug(String.format("Total time aggregatePerHour(): %s msec ", (lEndTime - lStartTime)));
	}

	@Override
	public void aggregatePerDay(String eventName, Date startDate, Date endDate) {
		long lStartTime = System.currentTimeMillis();

		Criteria criteria = new Criteria();
		criteria = prepareDateCriteria(criteria, "_id", startDate, endDate);
		metricMongoTemplate.mapReduce(
				new Query(criteria),
				getCollection(eventName, HOUR),
				"classpath:daily_map_function.js",
				"classpath:reduce_function.js",
				new MapReduceOptions().outputCollection(getCollection(eventName, DAY)).outputTypeMerge()
						.finalizeFunction("classpath:finalize_function.js"), HierarchialAggregationResult.class);

		long lEndTime = System.currentTimeMillis();
		logger.debug(String.format("Total time aggregatePerDay(): %s msec ", (lEndTime - lStartTime)));
	}

	@Override
	public void aggregatePerMonth(String eventName, Date startDate, Date endDate) {
		long lStartTime = System.currentTimeMillis();

		Criteria criteria = new Criteria();
		criteria = prepareDateCriteria(criteria, "_id", startDate, endDate);
		metricMongoTemplate.mapReduce(
				new Query(criteria),
				getCollection(eventName, DAY),
				"classpath:monthly_map_function.js",
				"classpath:reduce_function.js",
				new MapReduceOptions().outputCollection(getCollection(eventName, MONTH)).outputTypeMerge()
						.finalizeFunction("classpath:finalize_function.js"), HierarchialAggregationResult.class);

		long lEndTime = System.currentTimeMillis();
		logger.debug(String.format("Total time aggregatePerMonth(): %s msec ", (lEndTime - lStartTime)));
	}

	@Override
	public void aggregatePerYear(String eventName, Date startDate, Date endDate) {
		long lStartTime = System.currentTimeMillis();

		Criteria criteria = new Criteria();
		criteria = prepareDateCriteria(criteria, "_id", startDate, endDate);
		metricMongoTemplate.mapReduce(
				new Query(criteria),
				getCollection(eventName, MONTH),
				"classpath:yearly_map_function.js",
				"classpath:reduce_function.js",
				new MapReduceOptions().outputCollection(getCollection(eventName, YEAR)).outputTypeMerge()
						.finalizeFunction("classpath:finalize_function.js"), HierarchialAggregationResult.class);

		long lEndTime = System.currentTimeMillis();
		logger.debug(String.format("Total time aggregatePerYear(): %s msec ", (lEndTime - lStartTime)));
	}

	private Criteria prepareDateCriteria(Criteria criteria, String dateField, Date startDate, Date endDate) {
		if (startDate != null && endDate != null) {
			criteria.andOperator(Criteria.where(dateField).gte(startDate), Criteria.where(dateField).lt(endDate));
		} else if (startDate != null) {
			criteria.andOperator(Criteria.where(dateField).gte(startDate));
		} else if (endDate != null) {
			criteria.andOperator(Criteria.where(dateField).lt(endDate));
		}
		logger.debug("Criteria used for aggregation : " + criteria.getCriteriaObject());
		return criteria;
	}

	private String getCollection(String eventName, MetricResolution resolution) {
		return eventName.concat(".").concat(resolution.getCode());
	}

	private String getEventCollectionName(String eventName) {
		Assert.notNull(eventName, "eventName must not be null!");
		return eventName.concat(GaugeEventRepository.EVENT_COLLECTION);
	}

}
