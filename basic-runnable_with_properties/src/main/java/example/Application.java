package example;

import java.util.Properties;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.ApplicationContext;
import org.springframework.context.event.EventListener;
import org.springframework.core.task.SimpleAsyncTaskExecutor;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.stereotype.Component;

import example.task.EventLoggingCallableTask;
import example.task.EventLoggingTask;

@SpringBootApplication
@Component
@EnableScheduling
@SuppressWarnings({ "unused", "rawtypes" })
public class Application {
	private Logger logger = LoggerFactory.getLogger(Application.class);

	@Autowired
	EventLoggingTask eventLoggingTask;
	@Autowired
	EventLoggingCallableTask eventLoggingCallableTask;

	public static void main(String[] args) {
		SpringApplication.run(Application.class, args);
	}

	@EventListener(ApplicationReadyEvent.class)
	public void ready() throws InterruptedException, ExecutionException {
		// both work
		executeAsyncTask();
		executeCallableTask();
	}

	private void executeAsyncTask() {
		SimpleAsyncTaskExecutor executor = new SimpleAsyncTaskExecutor();
		executor.execute(eventLoggingTask);
	}

	// https://www.baeldung.com/java-runnable-callable
	// to return result, switch to Callable
	private void executeCallableTask() throws InterruptedException, ExecutionException {
		ExecutorService executorService = Executors.newSingleThreadExecutor();
		Future<String> future = executorService.submit(eventLoggingCallableTask);
		while (!future.isDone()) {
			logger.info("waiting for the task");
			Thread.sleep(100);
		}
		String result = future.get();
		logger.info("result: {}", result);
		executorService.shutdown();
		logger.info("service shut down: {}", executorService.isShutdown());
		

	}
}
