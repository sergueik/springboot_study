package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;

@Component
public class AsyncRunner implements CommandLineRunner {

	private static final Logger logger = LoggerFactory
			.getLogger(AsyncRunner.class);

	private final GithubLookupService gitHubLookupService;

	public AsyncRunner(GithubLookupService gitHubLookupService) {
		this.gitHubLookupService = gitHubLookupService;
	}

	@Override
	public void run(String... args) throws Exception {
		// Start the clock
		long start = System.currentTimeMillis();

		// Kick of multiple, asynchronous lookups
		List<String> logins = Arrays.asList(new String[] { "PivotalSoftware",
				"CloudFoundry", "Spring-Projects", "sergueik" });
		List<CompletableFuture<Follower>> pages = new ArrayList<>();
		for (String login : logins) {
			User user = new User();
			user.setLogin(login);
			CompletableFuture<Follower> page = gitHubLookupService.findUser(user);
			pages.add(page);
		}

		// Wait until they are all done
		CompletableFuture.allOf(pages.toArray(new CompletableFuture[logins.size()]))
				.join();

		// Print results, including elapsed time
		logger.info("Elapsed time: " + (System.currentTimeMillis() - start));
		for (CompletableFuture<Follower> page : pages) {
			logger.info("--> " + page.get());
		}
	}

}
