package net.arunoday.metric.store.service.impl;

import static net.arunoday.metric.store.model.MetricResolution.DAY;
import static net.arunoday.metric.store.model.MetricResolution.HOUR;
import static net.arunoday.metric.store.model.MetricResolution.MINUTE;
import static net.arunoday.metric.store.model.MetricResolution.MONTH;
import static net.arunoday.metric.store.model.MetricResolution.YEAR;
import net.arunoday.metric.store.model.HierarchialAggregationResult;
import net.arunoday.metric.store.repository.GaugeMetricRepository;

import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;

/**
 * REST service for gauge metric.
 * 
 * @author Aparna Chaudhary
 */
@RestController
@RequestMapping(value = "/metric")
public class GaugeMetricRestService {

	@Autowired
	private GaugeMetricRepository<String> metricRepository;

	// TODO: Date validations

	/**
	 * Provides the gauge metric for the given time.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @param day the day of the month, from 1 to 31
	 * @param hour the hour of the day, from 0 to 23
	 * @param minute the minute of the hour, from 0 to 59
	 * @return gauge metric for the given time
	 */
	@RequestMapping(value = "/minute/{eventId}/{year}/{month}/{day}/{hour}/{minute}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listMinuteAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month, @PathVariable("day") int day,
			@PathVariable("hour") int hour, @PathVariable("minute") int minute) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, day, hour, minute);
		DateTime endDate = startDate.plusMinutes(1);
		return metricRepository.find(eventId, MINUTE, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the minute-wise gauge metric for the given hour.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @param day the day of the month, from 1 to 31
	 * @param hour the hour of the day, from 0 to 23
	 * @return gauge metric aggregations for the given hour
	 */
	@RequestMapping(value = "/minute/{eventId}/{year}/{month}/{day}/{hour}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listMinuteAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month, @PathVariable("day") int day,
			@PathVariable("hour") int hour) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, day, hour, 0);
		DateTime endDate = startDate.plusHours(1);
		return metricRepository.find(eventId, MINUTE, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the minute-wise gauge metric for the given day.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @param day the day of the month, from 1 to 31
	 * @return gauge metric aggregations for the given day
	 */
	@RequestMapping(value = "/minute/{eventId}/{year}/{month}/{day}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listMinuteAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month, @PathVariable("day") int day) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, day, 0, 0);
		DateTime endDate = startDate.plusDays(1);
		return metricRepository.find(eventId, MINUTE, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the minute wise gauge metric for the given month.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @return gauge metric aggregations for the given month
	 */
	@RequestMapping(value = "/minute/{eventId}/{year}/{month}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listMinuteAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, 1, 0, 0);
		DateTime endDate = startDate.plusMonths(1);
		return metricRepository.find(eventId, MINUTE, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the hourly gauge metric for the given hour.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @param day the day of the month, from 1 to 31
	 * @param hour the hour of the day, from 0 to 23
	 * @return gauge metric aggregations for the given hour
	 */
	@RequestMapping(value = "/hourly/{eventId}/{year}/{month}/{day}/{hour}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listHourlyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month, @PathVariable("day") int day,
			@PathVariable("hour") int hour) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, day, hour, 0);
		DateTime endDate = startDate.plusHours(1);
		return metricRepository.find(eventId, HOUR, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the hourly gauge metric for the given day.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @param day the day of the month, from 1 to 31
	 * @return gauge metric aggregations for the given day
	 */
	@RequestMapping(value = "/hourly/{eventId}/{year}/{month}/{day}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listHourlyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month, @PathVariable("day") int day) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, day, 0, 0);
		DateTime endDate = startDate.plusDays(1);
		return metricRepository.find(eventId, HOUR, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the hourly gauge metric for the given month.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @return gauge metric aggregations for the given month
	 */
	@RequestMapping(value = "/hourly/{eventId}/{year}/{month}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listHourlyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, 1, 0, 0);
		DateTime endDate = startDate.plusMonths(1);
		return metricRepository.find(eventId, HOUR, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the daily gauge metric for the given day.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @param day the day of the month, from 1 to 31
	 * @return gauge metric aggregations for the given day
	 */
	@RequestMapping(value = "/daily/{eventId}/{year}/{month}/{day}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listDailyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month, @PathVariable("day") int day) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, day, 0, 0);
		DateTime endDate = startDate.plusDays(1);

		return metricRepository.find(eventId, DAY, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the daily gauge metric for the given month.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @return gauge metric aggregations for the given month
	 */
	@RequestMapping(value = "/daily/{eventId}/{year}/{month}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listDailyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, 1, 0, 0);
		DateTime endDate = startDate.plusMonths(1);

		return metricRepository.find(eventId, DAY, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the daily gauge metric for the given year.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @return gauge metric aggregations for the given year
	 */
	@RequestMapping(value = "/daily/{eventId}/{year}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listDailyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year) {
		validateYear(year);
		DateTime startDate = new DateTime(year, 1, 1, 0, 0);
		DateTime endDate = startDate.plusYears(1);

		return metricRepository.find(eventId, DAY, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the monthly gauge metric for the given month.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @param month the month of the year, from 1 to 12
	 * @return gauge metric aggregations for the given month
	 */
	@RequestMapping(value = "/monthly/{eventId}/{year}/{month}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listMonthlyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year, @PathVariable("month") int month) {
		validateYear(year);
		DateTime startDate = new DateTime(year, month, 1, 0, 0);
		DateTime endDate = startDate.plusMonths(1);
		return metricRepository.find(eventId, MONTH, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the monthly gauge metric for the given year.
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @return gauge metric aggregations for the given year
	 */
	@RequestMapping(value = "/monthly/{eventId}/{year}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listMonthlyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year) {
		validateYear(year);
		DateTime startDate = new DateTime(year, 1, 1, 0, 0);
		DateTime endDate = startDate.plusYears(1);
		return metricRepository.find(eventId, MONTH, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Provides the yearly aggregation
	 * 
	 * @param eventId name of the event
	 * @param year year in 4 digit format
	 * @return gauge metric aggregations for the given year
	 */
	@RequestMapping(value = "/yearly/{eventId}/{year}", method = RequestMethod.GET, produces = MediaType.APPLICATION_JSON_VALUE)
	public Iterable<HierarchialAggregationResult> listYearlyAggregations(@PathVariable("eventId") String eventId,
			@PathVariable("year") int year) {
		validateYear(year);
		DateTime startDate = new DateTime(year, 1, 1, 0, 0);
		DateTime endDate = startDate.plusYears(1);
		return metricRepository.find(eventId, YEAR, startDate.toDate(), endDate.toDate());
	}

	/**
	 * Maps exception to HTTP Response.
	 * 
	 * @param iae illegal argument exception
	 * @return validation error message
	 */
	@ExceptionHandler(IllegalArgumentException.class)
	@ResponseStatus(HttpStatus.BAD_REQUEST)
	public String handleInvalidInputException(IllegalArgumentException iae) {
		return "Validation Error: ".concat(iae.getMessage());
	}

	private void validateYear(int year) {
		if (year == 0) {
			throw new IllegalArgumentException("'year' cannot be zero.");
		}
	}

}
