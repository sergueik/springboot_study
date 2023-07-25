package example;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import example.task.EventLoggingTask;

public class Application {

	private static ExecutorService executorService;

	public static void main(String[] args) {
		executeTask();
	}

	private static void executeTask() {
		executorService = Executors.newSingleThreadExecutor();

		EventLoggingTask task = new EventLoggingTask();

		Future future = executorService.submit(task);

		executorService.shutdown();
	}
}
