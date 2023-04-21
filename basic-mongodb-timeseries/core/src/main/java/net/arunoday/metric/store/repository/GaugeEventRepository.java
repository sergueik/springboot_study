/**
 * 
 */
package net.arunoday.metric.store.repository;

import java.io.Serializable;
import java.util.Collection;
import java.util.Date;

import net.arunoday.metric.store.model.GaugeEvent;

import org.springframework.data.mongodb.core.query.Criteria;

/**
 * MongoDB Repository for {@link GaugeEvent}
 * 
 * @author Aparna Chaudhary
 */
public interface GaugeEventRepository<ID extends Serializable> {

	static final String EVENT_COLLECTION = ".event";

	/**
	 * Saves a given entity. Use the returned instance for further operations as the save operation might have changed the
	 * entity instance completely.
	 * 
	 * @param entity gauge event entity
	 * @return the saved entity
	 */
	GaugeEvent save(GaugeEvent entity);

	/**
	 * Retrieves an entity by its id and event name.
	 * 
	 * @param id must not be {@literal null}.
	 * @param eventType event name must not be {@literal null}.
	 * @return the entity with the given id or {@literal null} if none found
	 * @throws IllegalArgumentException if {@code id} is {@literal null}
	 */
	GaugeEvent findOne(ID id, String eventType);

	/**
	 * Returns all instances of the {@link GaugeEvent} by event type matching the filter criteria for the given time
	 * range.
	 * 
	 * @param eventType event name must not be {@literal null}.
	 * @param criteria for query execution
	 * @param startTime the start date for events, inclusive
	 * @param endTime the stop date for events, exclusive
	 * @param limit the maximum number of events to return; defaults to ten thousand
	 * @return matching event entities
	 */
	Iterable<GaugeEvent> find(String eventType, Criteria criteria, Date startTime, Date endTime, int limit);

	/**
	 * Returns all instances of the {@link GaugeEvent} by event type.
	 * 
	 * @param eventType event name
	 * @return all entities
	 */
	Iterable<GaugeEvent> findAll(String eventType);

	/**
	 * Returns stored event types
	 * 
	 * @return collection of stored event types
	 */
	Collection<String> findEventTypes();

	/**
	 * Returns the number of entities available for the given event name.
	 * 
	 * @param eventType event name must not be {@literal null}.
	 * @return the number of entities
	 */
	long count(String eventType);

	/**
	 * Deletes all entities managed by the repository for the given event name.
	 * 
	 * @param eventType event name must not be {@literal null}.
	 */
	void deleteAll(String eventType);

}
