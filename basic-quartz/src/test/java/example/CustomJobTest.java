package example;

import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.quartz.JobBuilder;
import org.quartz.JobDetail;
import org.quartz.Scheduler;
import org.quartz.SchedulerException;
import org.quartz.Trigger;
import org.quartz.TriggerBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.test.context.testng.AbstractTransactionalTestNGSpringContextTests;
import org.testng.annotations.Test;

@PropertySource("classpath:application.properties")
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = { Application.class })
public class CustomJobTest
		extends AbstractTransactionalTestNGSpringContextTests {
	@Autowired
	private Scheduler scheduler;
	private static final Logger LOG = LoggerFactory
			.getLogger(CustomJobTest.class);

	private Utils utils;

	@BeforeEach
	public void setup() throws InterruptedException, SchedulerException {
		utils = Utils.getInstance();
		LOG.info("Getting debug in setup: " + utils.getDebug());
		JobDetail jobDetail = JobBuilder.newJob(CustomJob.class).storeDurably(true)
				.build();

		Trigger trigger = TriggerBuilder.newTrigger().forJob(jobDetail).startNow()
				.build();

		scheduler.scheduleJob(jobDetail, trigger);

		Thread.sleep(1000);
	}

	// NOTE: setting utils isingleton in the setupand erifying in the test
	// leads to NPE in the test
	@Test
	public void test1() {

		Assertions.assertThrows(java.lang.NullPointerException.class, () -> {
			// utils = Utils.getInstance();
			LOG.info("Getting debug in test1: " + utils.getDebug());
		});
		assertThat(utils, nullValue());
	}

	// NOTE: get singleton that was modified from CustomJob
	@Test
	public void test2() {

		utils = Utils.getInstance();
		LOG.info("Getting debug in test2: " + utils.getDebug());
		LOG.info("Getting information in test2: " + utils.getInfo());
	}
}
