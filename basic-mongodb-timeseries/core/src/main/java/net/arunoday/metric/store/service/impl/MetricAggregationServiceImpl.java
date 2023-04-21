package net.arunoday.metric.store.service.impl;

import net.arunoday.metric.store.repository.GaugeEventRepository;
import net.arunoday.metric.store.repository.GaugeMetricRepository;
import net.arunoday.metric.store.service.MetricAggregationService;

import org.joda.time.DateTime;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Service;

/**
 * Default implementation of {@link MetricAggregationService}
 * 
 * @author Aparna Chaudhary
 */
@Service
public class MetricAggregationServiceImpl implements MetricAggregationService {

	@Autowired
	private GaugeEventRepository<String> gaugeEventRepository;
	@Autowired
	private GaugeMetricRepository<String> gaugeMetricRepository;

	@Override
	@Scheduled(cron = "0 0-59 * * * *")
	public void performAggregationPerMinute() {
		DateTime now = new DateTime();
		for (String eventType : gaugeEventRepository.findEventTypes()) {
			gaugeMetricRepository.aggregatePerMinute(eventType, now.minusSeconds(60).toDate(), now.toDate());
		}
	}

	@Override
	@Scheduled(cron = "0 0 * * * *")
	public void performAggregationPerHour() {
		DateTime now = new DateTime();
		for (String eventType : gaugeEventRepository.findEventTypes()) {
			gaugeMetricRepository.aggregatePerHour(eventType, now.minusMinutes(60).toDate(), now.toDate());
		}
	}

	@Override
	@Scheduled(cron = "0 0 0 * * *")
	public void performAggregationPerDay() {
		DateTime now = new DateTime();
		for (String eventType : gaugeEventRepository.findEventTypes()) {
			gaugeMetricRepository.aggregatePerDay(eventType, now.minusHours(24).toDate(), now.toDate());
		}
	}

	@Override
	@Scheduled(cron = "0 0 0 * * *")
	public void performAggregationPerMonth() {
		DateTime now = new DateTime();
		// start of month
		DateTime startDate = new DateTime(now.getYear(), now.getMonthOfYear(), 1, 0, 0);
		for (String eventType : gaugeEventRepository.findEventTypes()) {
			gaugeMetricRepository.aggregatePerMonth(eventType, startDate.toDate(), now.toDate());
		}
	}

	@Override
	@Scheduled(cron = "0 0 0 * * *")
	public void performAggregationPerYear() {
		DateTime now = new DateTime();
		// start of year
		DateTime startDate = new DateTime(now.getYear(), 1, 1, 0, 0);
		for (String eventType : gaugeEventRepository.findEventTypes()) {
			gaugeMetricRepository.aggregatePerYear(eventType, startDate.toDate(), now.toDate());
		}
	}
}
