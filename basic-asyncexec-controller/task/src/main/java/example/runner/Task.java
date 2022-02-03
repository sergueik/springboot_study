
package example.runner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.CommandLineRunner;

import org.springframework.stereotype.Component;

@Component
public class Task implements CommandLineRunner {

	// TODO: share instance with the controller
	private static final Logger logger = LoggerFactory.getLogger(Task.class);

	public Task() {
	}

	public void cancel() {
		logger.info(String.format("%s Processing cancel method",
				this.getClass().getName()));
	}

	@Override
	public void run(String... args) throws Exception {
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				cancel();
			}
		});
		Thread.sleep(120000);
	}
}
