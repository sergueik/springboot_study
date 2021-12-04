package example.tasks;

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

	// run every 3 sec
	@Scheduled(fixedRateString = "3000")
	public void send() throws ExecutionException, InterruptedException {
		logger.info(String.format("executed %d times", count.incrementAndGet()));
	}

	public int getCount() {
		return count.get();
	}
}