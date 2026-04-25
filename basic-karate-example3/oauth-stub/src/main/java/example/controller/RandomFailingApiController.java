package example.controller;

import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.Authentication;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api")
public class RandomFailingApiController {
	@Value("${example.codes:429}")
	private String failCodesProperty;

	// default is no error, if nonzero - fail the count times before returning ok
	@Value("${example.failcount:0}")
	private int failCount;

	private final AtomicInteger counter = new AtomicInteger();

	// retry behavior simulator
	@GetMapping("/hello")
	public ResponseEntity<String> hello(Authentication auth) {

		int currentAttempt = counter.incrementAndGet();

		List<HttpStatus> failStatuses = Stream.of(failCodesProperty.split(",")).map(String::trim).map(Integer::parseInt)
				.map(HttpStatus::valueOf).collect(Collectors.toList());

		if (currentAttempt <= failCount) {
			HttpStatus status = failStatuses.get((currentAttempt - 1) % failStatuses.size());

			if (status == HttpStatus.TOO_MANY_REQUESTS) {
				System.out.println("WARNING: throttled request attempt " + currentAttempt);
			}

			if (status == HttpStatus.FOUND) {
				return ResponseEntity.status(HttpStatus.FOUND).header("Location", "/api/hello").build();
			}

			return ResponseEntity.status(status).body("simulated " + status + " at attempt " + currentAttempt);
		}

		return ResponseEntity.ok("hello " + auth.getName());

	}
}