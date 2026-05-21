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

	private AtomicInteger count = new AtomicInteger(0);

	private Instant lastExecution = null;

	@Scheduled(cron = "${example.cron:0 * * * * *}")
	public void cron() {
		lastExecution = Instant.now();
		logger.info(String.format("executed at %s %d time%s", lastExecution.toEpochMilli(), count.get(),
				((count.incrementAndGet() == 2) ? "" : "s")));

	}

	public Instant getLastExecution() {
		return lastExecution;
	}

	public int getCount() {
		return count.get();
	}
}
