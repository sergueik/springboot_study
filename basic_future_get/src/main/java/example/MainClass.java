package example;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

import example.domain.Employee;
import example.threads.TaskAsCallable;

public class MainClass {

	public static void main(String[] args) throws InterruptedException, ExecutionException {

		ExecutorService executorService = Executors.newFixedThreadPool(5);

		List<Callable<Employee>> callables = new ArrayList<>();
		callables.add(new TaskAsCallable(1001));
		callables.add(new TaskAsCallable(1002));
		callables.add(new TaskAsCallable(1003));
		callables.add(new TaskAsCallable(1004));
		callables.add(new TaskAsCallable(1005));

		// returns a list of Futures holding their status when all complete
		List<Future<Employee>> tasks = executorService.invokeAll(callables);

		System.out.println(tasks.size() + " Responses recieved.\n");

		for (Future<Employee> task : tasks) {
			System.out.println(task.get().toString());
		}

		/* shutdown your thread pool, else your application will keep running */
		executorService.shutdown();

	}
}
