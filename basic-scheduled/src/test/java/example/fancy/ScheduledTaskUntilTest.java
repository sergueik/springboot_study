package example.fancy;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.awaitility.Awaitility.await;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;

import org.awaitility.Duration;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import example.tasks.ScheduledTask;

@SpringJUnitConfig(example.Application.class)
public class ScheduledTaskUntilTest {

	@SpyBean
	ScheduledTask task;

	@Test
	public void test() {
		await().atMost(Duration.ONE_MINUTE)
				.untilAsserted(() -> verify(task, atLeast(10)).send());
	}
}
