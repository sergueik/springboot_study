package example;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.scheduling.concurrent.ThreadPoolTaskExecutor;

import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletionException;
import java.util.concurrent.Executor;
import java.lang.IllegalStateException;


@SpringBootApplication
@EnableAsync
public class Application {

	public static void main(String[] args) {
		try {
			SpringApplication.run(Application.class, args).close();
		} catch (IllegalStateException e) {
			// catching just to reduce console output
		}
	}

	@Bean
	public Executor asyncExecutor() {
		ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
		executor.setCorePoolSize(2);
		executor.setMaxPoolSize(2);
		executor.setQueueCapacity(500);
		executor.setThreadNamePrefix("JDAsync-");
		executor.initialize();
		return executor;
	}
}
