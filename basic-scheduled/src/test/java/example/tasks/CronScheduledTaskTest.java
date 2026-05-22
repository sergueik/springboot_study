package example.tasks;

import static org.awaitility.Awaitility.await;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.doAnswer;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.notNullValue;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;

import java.time.Duration;
import java.time.Instant;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static java.util.concurrent.TimeUnit.SECONDS;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.scheduling.config.CronTask;
import org.springframework.test.context.TestPropertySource;

@SpringBootTest
@TestPropertySource(properties = { "example.cron=*/1 * * * * *" })
public class CronScheduledTaskTest {

	@SpyBean
	private CronScheduledTask task;
	private final AtomicInteger counter = new AtomicInteger();

	private final AtomicReference<Instant> lastExecution = new AtomicReference<>();

	@BeforeEach
	public void setUp() {

		// NOTE: when(task.cron())
		// does NOT compile for void methods

		doAnswer(invocation -> {
			// do misc. inventory bookkeeping
			counter.incrementAndGet();
			lastExecution.set(Instant.now());
			return invocation.callRealMethod();
		}).when(task).cron();

		doAnswer(invocation -> counter.get()).when(task).getCount();

		doAnswer(invocation -> lastExecution.get()).when(task).getLastExecution();
	}

	@DisplayName("Spring Should Run Cron Task")
	@Test
	void test1() {

		await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> {

			assertThat("cron invocation count", counter.get(), greaterThan(1));
			assertThat("recorded execution", lastExecution.get(), notNullValue());
			assertThat("alternative cron invocation count", task.getCount(), greaterThan(3));
			assertThat("recorded execution", task.getLastExecution(), notNullValue());
		});
	}

	@DisplayName("Spring Should Run Cron Task")
	@Test
	public void test2() {
		await().atMost(60, SECONDS).untilAsserted(() -> verify(task, atLeast(1)).cron());
	}

}