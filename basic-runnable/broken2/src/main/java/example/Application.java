package example;

import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.springframework.beans.factory.annotation.Autowired;

import example.task.EventLoggingTask;

public class Application {

	private static ExecutorService executorService;

	@Autowired
	public Properties properties;

	public static void main(String[] args) {
		executeTask();
	}

	private static void executeTask() {
		executorService = Executors.newSingleThreadExecutor();

		EventLoggingTask task = new EventLoggingTask(new Application().properties);

		Future future = executorService.submit(task);

		executorService.shutdown();
	}
}
