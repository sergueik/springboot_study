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
@PropertySource("classpath:application.properties")

public class BasicTest {

	@Autowired
	FixedRateScheduledTask task;

	@Test
	public void test() throws InterruptedException {
		Thread.sleep(7000L);
		assertThat(task.getCount(), greaterThan(2));
		assertThat(task.getCount(), lessThan(4));
	}

	@AfterEach
	public void after() {

	}
}