package example;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;


import org.slf4j.Logger;
import org.slf4j.LoggerFactory;


import org.junit.Test;

import example.TransactionFactory;
import example.Transfer;
import example.LogRunnable;

public class IntegrationTest {

	@Test
	public void main() throws InterruptedException {
		ExecutorService executor = Executors.newFixedThreadPool(3);
		TransactionFactory transactionFactory = new TransactionFactory();
		for (int i = 0; i < 10; i++) {
			Transfer tx = transactionFactory.newInstance();
			Runnable task = new LogRunnable(tx);
			executor.submit(task);
		}
		executor.shutdown();
		executor.awaitTermination(60, TimeUnit.SECONDS);
	}
}
