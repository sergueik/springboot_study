package net.arunoday.metric.store.repository;

import java.io.Serializable;
import java.util.Date;

import net.arunoday.metric.store.model.CounterEventEntity;
import net.arunoday.metric.store.model.MetricOperation;

/**
 * Defines custom repository operations for {@link CounterEventEntity}
 * 
 * @author Aparna Chaudhary
 */
public interface CounterEventRepository<ID extends Serializable> {

	/**
	 * Saves a given counter entity. Use the returned instance for further operations as the save operation might have
	 * changed the entity instance completely.
	 * 
	 * @param entity entity to save
	 * @param isDecrement counter is decremented if true; if false increment the existing counter by the current value
	 * @return the saved entity
	 */
	CounterEventEntity save(CounterEventEntity entity, boolean isDecrement);

	/**
	 * Retrieves an entity by its id and event name.
	 * 
	 * @param id must not be {@literal null}.
	 * @param eventType event name must not be {@literal null}.
	 * @return the entity with the given id or {@literal null} if none found
	 * @throws IllegalArgumentException if {@code id} is {@literal null}
	 */
	CounterEventEntity findOne(ID id, String eventType);

	/**
	 * Retrieves entities for the event name.
	 * 
	 * @param eventType event name must not be {@literal null}.
	 * @return the entities for the given event name
	 * @throws IllegalArgumentException if {@code id} is {@literal null}
	 */
	Iterable<CounterEventEntity> findAll(String eventType);

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

	/**
	 * Deletes all entities managed by the repository.
	 * 
	 */
	void deleteAll();

	/**
	 * Performs aggregation based on the input metric operation.
	 * 
	 * @param eventName name of the event for which aggregation is required
	 * @param metricOperation metric group operation
	 * @param startDate start date for event filtering; date is inclusive; ignored if null.
	 * @param endDate end date for event filtering; date is exclusive; ignored if null.
	 * @return result of aggregation
	 */
	Double performAggregation(String eventName, MetricOperation metricOperation, Date startDate, Date endDate);

}
