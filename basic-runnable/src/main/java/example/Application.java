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

	@Autowired
	private ApplicationContext applicationContext;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@EventListener(ApplicationReadyEvent.class)
	public void ready() {
		// both work
		executeAsyncTask();
		executeTask();
	}

	private void executeAsyncTask() {
		EventLoggingTask eventLoggingTask = applicationContext
				.getBean(EventLoggingTask.class);
		// NOTE: constructing Task directly leads to NPE in accessing the Config
		// EventLoggingTask eventLoggingTask = new EventLoggingTask();
		SimpleAsyncTaskExecutor executor = new SimpleAsyncTaskExecutor();
		executor.execute(eventLoggingTask);
	}

	private void executeTask() {
		ExecutorService executorService = Executors.newSingleThreadExecutor();
		// NOTE: constructing Task directly leads to NPE in accessing the Config
		EventLoggingTask eventLoggingTask = applicationContext
				.getBean(EventLoggingTask.class);
		Future future = executorService.submit(eventLoggingTask);
		executorService.shutdown();
	}
}
