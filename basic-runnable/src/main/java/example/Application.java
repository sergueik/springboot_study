package example;

import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import org.springframework.beans.factory.annotation.Autowired;

import example.task.EventLoggingTask;

@SuppressWarnings({ "unused", "rawtypes" })
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
