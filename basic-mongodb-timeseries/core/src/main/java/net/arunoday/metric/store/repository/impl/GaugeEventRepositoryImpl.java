package net.arunoday.metric.store.repository.impl;

import static net.arunoday.metric.store.model.GaugeEvent.OCCURED_ON_FIELD;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.List;

import net.arunoday.metric.store.model.GaugeEvent;
import net.arunoday.metric.store.repository.GaugeEventRepository;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.query.Criteria;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.stereotype.Repository;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Default MongoDB implementation for {@link GaugeEventRepository}
 * 
 * @author Aparna Chaudhary
 */
@Repository
public class GaugeEventRepositoryImpl implements GaugeEventRepository<String> {

	@Autowired
	@Qualifier("eventMongoTemplate")
	private MongoTemplate mongoTemplate;

	@Override
	public GaugeEvent save(GaugeEvent entity) {
		mongoTemplate.save(entity, getCollectionName(entity.getEventType()));
		return entity;
	}

	@Override
	public GaugeEvent findOne(String id, String eventType) {
		return mongoTemplate.findById(id, GaugeEvent.class, getCollectionName(eventType));
	}

	@Override
	public Iterable<GaugeEvent> findAll(String eventType) {
		return mongoTemplate.find(new Query(), GaugeEvent.class, getCollectionName(eventType));
	}

	@Override
	public Iterable<GaugeEvent> find(String eventType, Criteria criteria, Date startTime, Date endTime, int limit) {
		Query query = new Query(criteria);
		query.addCriteria(new Criteria(OCCURED_ON_FIELD).gte(startTime).lt(endTime));
		query.limit(limit);
		return mongoTemplate.find(query, GaugeEvent.class, getCollectionName(eventType));
	}

	@Override
	public Collection<String> findEventTypes() {
		List<String> collections = new ArrayList<String>();
		for (String collectionName : mongoTemplate.getCollectionNames()) {
			if (StringUtils.endsWithIgnoreCase(collectionName, EVENT_COLLECTION)) {
				collections.add(StringUtils.delete(collectionName, EVENT_COLLECTION));
			}
		}
		return collections;
	}

	@Override
	public long count(String eventName) {
		return mongoTemplate.getCollection(getCollectionName(eventName)).count();
	}

	@Override
	public void deleteAll(String eventName) {
		mongoTemplate.remove(new Query(), getCollectionName(eventName));
	}

	protected String getCollectionName(String eventName) {
		Assert.notNull(eventName, "eventName must not be null!");
		return eventName.concat(EVENT_COLLECTION);
	}

}
