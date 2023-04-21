package net.arunoday.metric.store.repository.impl;

import static net.arunoday.metric.store.model.CounterEventEntity.EVENT_TYPE_FIELD;
import static net.arunoday.metric.store.model.CounterEventEntity.OCCURED_ON_FIELD;
import static net.arunoday.metric.store.model.CounterEventEntity.TOTAL_COUNT_FIELD;
import static org.springframework.data.mongodb.core.query.Criteria.where;

import java.util.Date;

import net.arunoday.metric.store.model.CounterEventEntity;
import net.arunoday.metric.store.model.MetricOperation;
import net.arunoday.metric.store.repository.CounterEventRepository;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.domain.Sort;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.aggregation.Aggregation;
import org.springframework.data.mongodb.core.aggregation.AggregationOperation;
import org.springframework.data.mongodb.core.aggregation.AggregationResults;
import org.springframework.data.mongodb.core.aggregation.GroupOperation;
import org.springframework.data.mongodb.core.aggregation.MatchOperation;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;

import com.mongodb.DBObject;

/**
 * Default MongoDB implementation for {@link CounterEventRepository}
 * 
 * @author Aparna Chaudhary
 */
@Repository
public class CounterEventRepositoryImpl implements CounterEventRepository<String> {

	private static final Logger logger = LoggerFactory.getLogger(CounterEventRepositoryImpl.class);

	@Autowired
	@Qualifier("eventMongoTemplate")
	private MongoTemplate mongoTemplate;

	@Override
	public CounterEventEntity save(CounterEventEntity entity, boolean isDecrement) {
		Assert.notNull(entity, "The given entity must not be null!");

		Query query = new Query(where(EVENT_TYPE_FIELD).is(entity.getEventType()));
		query = query.with(new Sort(Sort.Direction.DESC, OCCURED_ON_FIELD));
		query.limit(1);

		CounterEventEntity existingEntity = mongoTemplate.findOne(query, CounterEventEntity.class);
		double existingCount = 0;
		if (existingEntity != null) {
			existingCount = existingEntity.getTotalCount();
		}
		if (isDecrement) {
			entity.setTotalCount(existingCount - entity.getTotalCount());
		} else {
			entity.setTotalCount(existingCount + entity.getTotalCount());
		}
		mongoTemplate.save(entity);
		return entity;
	}

	@Override
	public CounterEventEntity findOne(String id, String eventType) {
		return mongoTemplate.findById(id, CounterEventEntity.class);
	}

	@Override
	public Iterable<CounterEventEntity> findAll(String eventType) {
		return mongoTemplate.find(new Query(where(EVENT_TYPE_FIELD).is(eventType)), CounterEventEntity.class);
	}

	@Override
	public long count(String eventType) {
		return mongoTemplate.count(new Query(where(EVENT_TYPE_FIELD).is(eventType)), CounterEventEntity.class);
	}

	@Override
	public void deleteAll(String eventType) {
		mongoTemplate.remove(new Query(where(EVENT_TYPE_FIELD).is(eventType)), CounterEventEntity.class);
	}

	@Override
	public void deleteAll() {
		mongoTemplate.remove(new Query(), CounterEventEntity.class);
	}

	@Override
	public Double performAggregation(String eventName, MetricOperation metricOperation, Date startDate, Date endDate) {
		long lStartTime = System.currentTimeMillis();

		Criteria criteria = Criteria.where(EVENT_TYPE_FIELD).is(eventName);
		if (startDate != null && endDate != null) {
			criteria.andOperator(Criteria.where(OCCURED_ON_FIELD).gte(startDate), Criteria.where(OCCURED_ON_FIELD)
					.lt(endDate));
		} else if (startDate != null) {
			criteria.andOperator(Criteria.where(OCCURED_ON_FIELD).gte(startDate));
		} else if (endDate != null) {
			criteria.andOperator(Criteria.where(OCCURED_ON_FIELD).lt(endDate));
		}
		MatchOperation matchOperation = Aggregation.match(criteria);

		GroupOperation groupOperation = Aggregation.group(EVENT_TYPE_FIELD).min(TOTAL_COUNT_FIELD)
				.as(MetricOperation.MIN.getOperation()).max(TOTAL_COUNT_FIELD).as(MetricOperation.MAX.getOperation())
				.avg(TOTAL_COUNT_FIELD).as(MetricOperation.AVG.getOperation()).sum(TOTAL_COUNT_FIELD)
				.as(MetricOperation.SUM.getOperation());

		AggregationOperation[] operations = { matchOperation, groupOperation, Aggregation.limit(1) };
		AggregationResults<DBObject> result = mongoTemplate.aggregate(Aggregation.newAggregation(operations),
				CounterEventEntity.class, DBObject.class);

		Double resultValue = (Double) result.getUniqueMappedResult().get(metricOperation.getOperation());

		long lEndTime = System.currentTimeMillis();
		logger.debug(String.format("Total Time performAggregation(): %s msec ", (lEndTime - lStartTime)));

		return resultValue;
	}

}
