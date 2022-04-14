package com.kaviddiss.bootquartz;

import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import com.kaviddiss.bootquartz.job.SampleJob;
import org.quartz.*;
import org.springframework.beans.factory.annotation.Autowired;
// import org.springframework.boot.test.SpringApplicationConfiguration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.testng.AbstractTransactionalTestNGSpringContextTests;
import org.testng.annotations.Test;

// @SpringApplicationConfiguration(classes = Application.class)
@PropertySource("classpath:application.properties")
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = { Application.class })
public class ApplicationTest
		extends AbstractTransactionalTestNGSpringContextTests {
	// No qualifying bean of type 'org.quartz.Scheduler' available: expected at
	// least 1 bean which qualifies as autowire candidate. Dependency annotations:
	// {@org.springframework.beans.factory.annotation.Autowired(required=true)}
	@Autowired
	private Scheduler scheduler;

	@Test
	public void test() throws Exception {

		JobDetail jobDetail = JobBuilder.newJob(SampleJob.class).storeDurably(true)
				.build();

		Trigger trigger = TriggerBuilder.newTrigger().forJob(jobDetail).startNow()
				.build();

		scheduler.scheduleJob(jobDetail, trigger);

		Thread.sleep(10000);
	}
}
