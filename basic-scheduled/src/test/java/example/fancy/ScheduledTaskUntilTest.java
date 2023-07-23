package example.fancy;
/**
 * Copyright 2021,2023 Serguei Kouzmine
 */

import static org.awaitility.Awaitility.await;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.lessThan;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;

import org.awaitility.Duration;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import example.tasks.ScheduledTask;

@SpringJUnitConfig(example.Application.class)
@PropertySource("classpath:application.properties")

public class ScheduledTaskUntilTest {

	@SpyBean
	ScheduledTask task;
	private static int num = 15;

	@Test
	public void test() {
		await().atMost(Duration.ONE_MINUTE)
				.untilAsserted(() -> verify(task, atLeast(num)).send());
		// TODO: check the time spent running tasks
		// assertThat(task.getCount(), lessThan(30));
	}
}

