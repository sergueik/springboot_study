package example.tasks;

/**
 * Copyright 2021,2023, 2026 Serguei Kouzmine
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
	// originally @Scheduled(fixedRateString = "${example.fixed-rate:60000}")
	@Scheduled(fixedRateString = "${example.fixed-rate:5000}")
	public void fixedRate() throws ExecutionException, InterruptedException {
		logger.info(String.format("executing %d time%s", count.get(), ((count.incrementAndGet() == 2) ? "" : "s")));
	}

	// Method getCount() and property count are added solely
	// for test instrumentation purposes.
	//
	// Mockito spy/mock proxies cannot expose methods that do
	// not already exist on the proxied subject class.
	//
	// Alternatively, remove the count property from the subject
	// entirely and keep the counter state inside the spy/test layer.
	private final AtomicInteger count = new AtomicInteger(0);

	public int getCount() {
		return count.get();
	}
}
