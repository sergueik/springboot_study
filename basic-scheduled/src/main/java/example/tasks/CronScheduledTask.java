package example.tasks;

/**
 * Copyright 2026 Serguei Kouzmine
 */
import java.time.Instant;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.atomic.AtomicInteger;

@Component
public class CronScheduledTask {
	private final Logger logger = LoggerFactory.getLogger(CronScheduledTask.class);

	@Scheduled(cron = "${example.cron:0 * * * * *}")
	public void cron() {
		logger.info(String.format("executed cron() at %s", Instant.now().toEpochMilli()));
	}

	// Methods added solely for test instrumentation:
	// Mockito spy/mock proxies cannot expose methods
	// that do not already exist on the subject class.
	public Instant getLastExecution() {
		return null;
	}

	public int getCount() {
		return 0;
	}
}
