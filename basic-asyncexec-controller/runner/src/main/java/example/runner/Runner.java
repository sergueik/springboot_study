package example.runner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.CommandLineRunner;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
// NOTE: do not name the exaple.runner.ApplicationRunner;
// import org.springframework.boot.ApplicationRunner;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletionException;

import org.springframework.stereotype.Component;

import java.util.concurrent.CompletableFuture;

@Component
public class Runner implements CommandLineRunner {

	private Process process;
	private Runtime runTime;
	private static final Logger logger = LoggerFactory.getLogger(Runner.class);

	public Runner() {
	}

	public void cancel() {
		logger.info(String.format("%s Processing cancel method",
				this.getClass().getName()));

		process.destroy();
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
		}
		if (!process.isAlive()) {
			logger.info("task process destroyed");

		}
	}

	// based on:
	@Override
	public void run(String... args) throws Exception {
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				cancel();
			}
		});
		try {
			runTime = Runtime.getRuntime();
			logger.info("running task process");
			process = runTime.exec(
					"java -jar C:\\developer\\sergueik\\springboot_study\\basic-asyncexec-controller\\task\\target\\task-0.2.0-SNAPSHOT.jar");
			InputStream inputStream = process.getInputStream();
			InputStreamReader isr = new InputStreamReader(inputStream);
			InputStream errorStream = process.getErrorStream();
			InputStreamReader esr = new InputStreamReader(errorStream);

			int n1;
			char[] c1 = new char[1024];
			StringBuffer standardOutput = new StringBuffer();
			while ((n1 = isr.read(c1)) > 0) {
				standardOutput.append(c1, 0, n1);
			}
			System.out.println("Standard Output: " + standardOutput.toString());

			int n2;
			char[] c2 = new char[1024];
			StringBuffer standardError = new StringBuffer();
			while ((n2 = esr.read(c2)) > 0) {
				standardError.append(c2, 0, n2);
			}
			System.out.println("Standard Error: " + standardError.toString());
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
}
