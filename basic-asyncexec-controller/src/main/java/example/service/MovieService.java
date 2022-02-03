package example.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.web.client.RestTemplateBuilder;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;

import java.util.concurrent.CancellationException;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionException;

import example.model.MovieModel;

@Service
public class MovieService {

	private static final Logger logger = LoggerFactory
			.getLogger(MovieService.class);

	private final RestTemplate restTemplate;

	public MovieService(RestTemplateBuilder restTemplateBuilder) {
		this.restTemplate = restTemplateBuilder.build();
	}

	@Async
	public CompletableFuture<MovieModel> lookForMovie(String movieId)
			throws InterruptedException {

		try {
			logger.info("Looking up Movie ID: {}", movieId);
			String url = String.format("https://ghibliapi.herokuapp.com/films/%s",
					movieId);
			MovieModel results = restTemplate.getForObject(url, MovieModel.class);

			Thread.sleep(120000);
			return CompletableFuture.completedFuture(results);
		} catch (CompletionException | CancellationException e) {
			System.err.println("ignoring exception" + e.getCause().toString());
			return null;
		}

	}
}
