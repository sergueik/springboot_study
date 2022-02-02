package example.runner;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.boot.CommandLineRunner;
// NOTE: do not name the exaple.runner.ApplicationRunner;
// import org.springframework.boot.ApplicationRunner;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletionException;

import org.springframework.stereotype.Component;

import java.util.concurrent.CompletableFuture;

import example.service.MovieService;
import example.state.Data;
import example.model.MovieModel;

@Component
public class CustomApplicationRunner implements CommandLineRunner {

	private CompletableFuture<MovieModel> page1;
	private CompletableFuture<MovieModel> page2;
	private CompletableFuture<MovieModel> page3;

	// TODO: share instance with the controller
	private static final Logger logger = LoggerFactory
			.getLogger(CustomApplicationRunner.class);

	private final MovieService movieService;
	private Data data = Data.getInstance();

	public CustomApplicationRunner(MovieService movieService) {
		// NOTE: no synchronization
		data.setApplicationRunner(this);
		this.movieService = movieService;
	}

	public void cancel() {
		logger.info("Processing cancel method");
		final boolean mayInterruptIfRunning = true;
		page1.cancel(mayInterruptIfRunning);
		page2.cancel(mayInterruptIfRunning);
		page3.cancel(mayInterruptIfRunning);
	}

	@Override
	public void run(String... args) throws Exception {
		Runtime.getRuntime().addShutdownHook(new Thread() {
			@Override
			public void run() {
				cancel();
			}
		});
		long start = System.currentTimeMillis();

		page1 = movieService.lookForMovie("58611129-2dbc-4a81-a72f-77ddfc1b1b49");
		page2 = movieService.lookForMovie("2baf70d1-42bb-4437-b551-e5fed5a87abe");
		page3 = movieService.lookForMovie("4e236f34-b981-41c3-8c65-f8c9000b94e7");

		// Join all threads so that we can wait until all are done
		// NOTE: it appears impossible to catch CancellationException
		CompletableFuture.allOf(page1, page2, page3).join();

		// Print results, including elapsed time
		logger.info("Elapsed time: " + (System.currentTimeMillis() - start));
		logger.info("--> " + page1.get());
		logger.info("--> " + page2.get());
		logger.info("--> " + page3.get());
	}
}
