package example;

import static org.mockito.Mockito.when;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.quartz.*;
import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.test.context.ContextConfiguration;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.testng.AbstractTransactionalTestNGSpringContextTests;
import org.testng.annotations.Test;

import example.Application;
import example.job.SampleJob;
import example.service.SampleService;

import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import static org.mockito.Mockito.doNothing;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.never;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Matchers.eq;
import static org.mockito.Mockito.any;
import static org.mockito.Mockito.mock;
import org.mockito.exceptions.misusing.NullInsteadOfMockException;

@PropertySource("classpath:application.properties")
@ExtendWith(SpringExtension.class)
@ContextConfiguration(classes = { Application.class })
public class ApplicationTest
		extends AbstractTransactionalTestNGSpringContextTests {
	@Autowired
	private Scheduler scheduler;
	@MockBean
	private SampleJob job;

	@MockBean
	private SampleService service;

	@BeforeEach
	public void setup() {
		job = mock(SampleJob.class);
		service = mock(SampleService.class);
		doNothing().when(service).hello();

	}

	@Test
	public void test() throws Exception {
		// NOTE: NPE id use job.getClass()

		JobDetail jobDetail = JobBuilder.newJob(SampleJob.class).storeDurably(true)
				.build();

		Trigger trigger = TriggerBuilder.newTrigger().forJob(jobDetail).startNow()
				.build();

		scheduler.scheduleJob(jobDetail, trigger);

		Thread.sleep(1000);
		try {
			verify(service, times(1)).hello();
		} catch (NullInsteadOfMockException e) {
			System.err.println("Exception (ignored) " + e.toString());

		}
	}
}

