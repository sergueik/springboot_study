package example.tasks;

/**
 * Copyright 2021,2023,2026 Serguei Kouzmine
 */

import static org.awaitility.Awaitility.await;
import org.awaitility.core.ConditionFactory;

import java.time.Duration;
import static java.util.concurrent.TimeUnit.SECONDS;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.lessThan;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;

// The import org.awaitility.Duration cannot be resolved - no longer exist in 4.x+ 
// import org.awaitility.Duration;
import java.time.Duration;
import org.junit.jupiter.api.Test;

import org.springframework.boot.convert.DurationStyle;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import example.tasks.FixedRateScheduledTask;

@SpringJUnitConfig(example.Application.class)
@PropertySource("classpath:application.yaml")
public class FixedRateScheduledTaskTest {

	@SpyBean
	FixedRateScheduledTask task;
	private static int num = 2;

	@Test
	public void test() {
		await().atMost(10, SECONDS).untilAsserted(() -> verify(task, atLeast(num)).fixedRate());
		// TODO: check the time spent running tasks
		assertThat("task run too many times: " + task.getCount(), task.getCount(), lessThan(3));
	}
}
