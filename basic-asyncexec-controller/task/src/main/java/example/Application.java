package example;

import org.springframework.boot.ExitCodeGenerator;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.ConfigurableApplicationContext;
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
		ConfigurableApplicationContext ctx = SpringApplication
				.run(Application.class, args);
		// ...determine it's time to stop...
		int exitCode = SpringApplication.exit(ctx, new ExitCodeGenerator() {
			@Override
			public int getExitCode() {
				// no errors
				return 0;
			}
		});
		System.err
				.println(String.format("%s exiting", Application.class.getName()));

		System.exit(exitCode);
	}

	@Bean
	public Executor asyncExecutor() {
		ThreadPoolTaskExecutor executor = new ThreadPoolTaskExecutor();
		executor.setCorePoolSize(2);
		executor.setMaxPoolSize(2);
		executor.setQueueCapacity(500);
		executor.setThreadNamePrefix("task-");
		executor.initialize();
		return executor;
	}
}
