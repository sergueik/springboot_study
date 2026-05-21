package example.tasks;

import static org.awaitility.Awaitility.await;
import static org.mockito.Mockito.atLeast;
import static org.mockito.Mockito.verify;
import static java.util.concurrent.TimeUnit.SECONDS;
import static org.assertj.core.api.Assertions.assertThat;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.TestInstance.Lifecycle;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;

import java.time.Duration;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.SpyBean;
import org.springframework.scheduling.config.CronTask;
import org.springframework.test.context.TestPropertySource;

@SpringBootTest
@TestPropertySource(properties = { "example.cron=*/1 * * * * *" })
public class CronScheduledTaskTest {

	@Autowired
	private CronScheduledTask task;

	@SpyBean
	private CronScheduledTask taskMock;

	@DisplayName("Spring Should Run Cron Task")
	@Test
	void test1() {

		await().atMost(Duration.ofSeconds(5)).untilAsserted(() -> {
			assertThat(task.getCount()).isGreaterThan(3);
			assertThat(task.getLastExecution()).isNotNull();
		});
	}

	
	@DisplayName("Spring Should Run Cron Task")
	@Test
	public void test2() {
		await().atMost(60, SECONDS).untilAsserted(() -> verify(taskMock, atLeast(1)).cron());
	}

}