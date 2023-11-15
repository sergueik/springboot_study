package example;

import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;

import example.task.EventLoggingTask;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.EventListener;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.stereotype.Component;

@SpringBootApplication
@Component
@EnableScheduling
@SuppressWarnings({ "unused", "rawtypes" })
public class Application {

	// does not work
	private static ExecutorService executorService;
	private SimpleAsyncTaskExecutor executor;

	@Autowired
	private ApplicationContext applicationContext;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@EventListener(ApplicationReadyEvent.class)
	public void ready() {
		EventLoggingTask eventLoggingTask = applicationContext
				.getBean(EventLoggingTask.class);
		executor = new SimpleAsyncTaskExecutor();
		executor.execute(eventLoggingTask);
		// executeTask();
	}

	private static void executeTask() {
		executorService = Executors.newSingleThreadExecutor();

		EventLoggingTask task = new EventLoggingTask();

		Future future = executorService.submit(task);

		executorService.shutdown();
	}
}
