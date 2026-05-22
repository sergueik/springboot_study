package example.tasks;
/**
 * Copyright 2021,2023,2026 Serguei Kouzmine
 */

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;

import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.junit.jupiter.SpringJUnitConfig;

@SpringJUnitConfig(example.Application.class)
// NOTE: one should NOT use @PropertySource for YAML - it does not work
// @PropertySource("classpath:application.yaml")

public class BasicTest {

	@Autowired
	FixedRateScheduledTask task;

	@Test
	public void test() throws InterruptedException {
		Thread.sleep(10000L);
		assertThat("task run too few times: " + task.getCount(),task.getCount(), greaterThan(2));
		assertThat("task run too many times: " + task.getCount(), task.getCount(), lessThan(4));
	}

	@AfterEach
	public void after() {

	}
}