package example;

import example.model.Tweet;
import example.repository.TweetRepository;

import org.assertj.core.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.reactive.AutoConfigureWebTestClient;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.reactive.server.FluxExchangeResult;
import org.springframework.test.web.reactive.server.WebTestClient;
import reactor.core.publisher.Mono;
import reactor.test.StepVerifier;

import java.util.Collections;
import reactor.core.publisher.Flux;
import reactor.core.publisher.Mono;

@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.RANDOM_PORT)
@AutoConfigureWebTestClient
class ControllerTest {

	@Autowired
	private WebTestClient webTestClient;

	@Autowired
	private TweetRepository tweetRepository;

	@BeforeEach
	void setup() {
		// clear embedded Mongo before each test
		tweetRepository.deleteAll().block();

		// insert some documents; block() ok in test setup
		tweetRepository.saveAll(Flux.just(new Tweet("tweet 1"), new Tweet("tweet 2"), new Tweet("tweet 3")))
				.blockLast();
	}

	@Test
	void test1() {

		// Act
		FluxExchangeResult<Tweet> result = webTestClient.get().uri("/tweets").accept(MediaType.APPLICATION_JSON)
				.exchange().expectStatus().isOk().returnResult(Tweet.class);

		// Extract Flux<Tweet>
		Flux<Tweet> body = result.getResponseBody();

		// Assert the reactive stream using StepVerifier
		StepVerifier.create(body).expectNextMatches(t -> t.getText().equals("tweet 1"))
				.expectNextMatches(t -> t.getText().equals("tweet 2"))
				.expectNextMatches(t -> t.getText().equals("tweet 3")).verifyComplete();
	}
}