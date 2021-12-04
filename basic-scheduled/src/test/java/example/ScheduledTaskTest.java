package example;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

import example.tasks.ScheduledTask;

@SpringJUnitConfig(example.Application.class)
public class ScheduledTaskTest {

	@Autowired
	ScheduledTask task;

	@Test
	public void test()
			throws InterruptedException {
		Thread.sleep(100L);
		assertThat(task.getCount(), greaterThan(0));
	}
}
