package net.arunoday.metric.store.service;


/**
 * Service API for metric aggregation.
 * 
 * @author Aparna Chaudhary
 */
public interface MetricAggregationService {

	/**
	 * Calculate aggregates per minute and store in metrics database with {@code eventName.minute} collection.
	 */
	void performAggregationPerMinute();

	/**
	 * Calculate aggregates per hour and store in metrics database with {@code eventName.hourly} collection.
	 */
	void performAggregationPerHour();

	/**
	 * Calculate aggregates per day and store in metrics database with {@code eventName.daily} collection. The aggregates
	 * are calculated every midnight.
	 */
	void performAggregationPerDay();

	/**
	 * Calculate monthly aggregates and store in metrics database with {@code eventName.monthly} collection. The
	 * aggregates are calculated every midnight.
	 */
	void performAggregationPerMonth();

	/**
	 * Calculate aggregates per year and store in metrics database with {@code eventName.yearly} collection. The
	 * aggregates are calculated every midnight.
	 */
	void performAggregationPerYear();
}
