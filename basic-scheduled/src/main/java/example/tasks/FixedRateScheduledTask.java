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
public class FixedRateScheduledTask {
	private final Logger logger = LoggerFactory.getLogger(FixedRateScheduledTask.class);

	private AtomicInteger count = new AtomicInteger(0);

	@Scheduled(fixedRateString = "${example.rate:60000}")
	public void fixedRate() throws ExecutionException, InterruptedException {
		logger.info(String.format("executing %d time%s", count.get(), ((count.incrementAndGet() == 2) ? "" : "s")));
	}

	public int getCount() {
		return count.get();
	}
}
