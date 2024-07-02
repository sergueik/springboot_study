package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.CompletableFuture;
import java.util.function.Function;
import java.util.function.Supplier;

@Service
public class GithubLookupService {

	private static final Logger logger = LoggerFactory.getLogger(GithubLookupService.class);

	private final RestTemplate restTemplate;

	public GithubLookupService(RestTemplateBuilder restTemplateBuilder) {
		this.restTemplate = restTemplateBuilder.build();
	}

	public User doUser(User user) {
		User result = null;
		logger.info("Looking up " + user.getLogin());
		String url = String.format("https://api.github.com/users/%s", user.getLogin());
		try {
			// de-serialize User object from JSON response
			result = restTemplate.getForObject(url, User.class);
			Thread.sleep(1000L);
		} catch (InterruptedException e) {
			//
		}
		return result;
	}

	public Follower doFollowers(User user) {
		List<Follower> result = new ArrayList<>();
		logger.info("Looking up followers of " + user.getLogin());
		String url = String.format("https://api.github.com/users/%s/followers", user.getLogin());
		try {
			// de-serialize User object from JSON response
			result = (List<Follower>) Arrays.asList(restTemplate.getForObject(url, Follower[].class));
			Thread.sleep(1000L);
		} catch (InterruptedException e) {
			//
		}
		for (int cnt = 0; cnt != result.size(); cnt++) {
			Follower follower = result.get(cnt);
			logger.info("Follower " + follower);
		}
		return result.size() == 0 ? null : result.get(0);
	}

	@Async
	public CompletableFuture<Follower> findUser(User user) throws InterruptedException {
		Supplier<User> supplier = () -> doUser(user);
		Function<User, Follower> function = (User o) -> {
			return doFollowers(o);
		};
		// NOTE: thenRun,thenAccept,thenApply,thenCompose,thenCombine,thenCombine  
		return CompletableFuture.supplyAsync(supplier).thenApplyAsync(function);
		// return CompletableFuture.supplyAsync(() ->
		// doUser(user)).thenApplyAsync((User o) -> doFollowers(o));
	}

	@Async
	public CompletableFuture<User> findUser(String user) throws InterruptedException {
		logger.info("Looking up " + user);
		String url = String.format("https://api.github.com/users/%s", user);
		// de-serialize User object from JSON response
		User results = restTemplate.getForObject(url, User.class);
		// extra delay of 1s for demonstration purposes
		Thread.sleep(1000L);
		return CompletableFuture.completedFuture(results);
	}

}
