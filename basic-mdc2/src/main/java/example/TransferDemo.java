package example;

// https://github.com/eugenp/tutorials/blob/master/logging-modules/log-mdc/src/main/java/com/baeldung/mdc/pool/MdcAwareThreadPoolExecutor.java
// import example.MdcAwareThreadPoolExecutor;
import example.LogRunnable;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.ThreadPoolExecutor.AbortPolicy;

import static java.util.concurrent.TimeUnit.MINUTES;

public class TransferDemo {

	public static void main(String[] args) {

		ExecutorService executor = new ThreadPoolExecutor(3, 3, 0, MINUTES,
				new LinkedBlockingQueue<>(), Thread::new, new AbortPolicy());

		TransactionFactory transactionFactory = new TransactionFactory();

		for (int i = 0; i < 10; i++) {
			Transfer tx = transactionFactory.newInstance();

			Runnable task = new LogRunnable(tx);

			executor.submit(task);
		}

		executor.shutdown();

	}
}
