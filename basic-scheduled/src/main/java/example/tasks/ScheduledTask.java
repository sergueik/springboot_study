package example.tasks;
/**
 * Copyright 2021,2023 Serguei Kouzmine
 */

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;

@Component
public class ScheduledTask {
	private final Logger logger = LoggerFactory.getLogger(ScheduledTask.class);

	private AtomicInteger count = new AtomicInteger(0);

	// NOTE: Exactly one of the cron(), fixedDelay(), or fixedRate() attributes
	// must be specified.
	// see also:
	// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/scheduling/annotation/Scheduled.html
	// run every "example.rate" sec
	// NOTE: set default to every 60 sec, and a mish shorter through properties
	// for a clear visually different rate
	@Scheduled(fixedRateString = "${example.rate:60000}")
	public void send() throws ExecutionException, InterruptedException {
		logger.info(String.format("executing %d time%s", count.get(),
				((count.incrementAndGet() == 2) ? "" : "s")));
	}

	public int getCount() {
		return count.get();
	}
}
