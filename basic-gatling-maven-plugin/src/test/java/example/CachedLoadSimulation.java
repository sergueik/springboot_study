package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.*;

import io.gatling.javaapi.core.*;
import io.gatling.javaapi.http.*;

import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

import java.util.Map;
import java.util.HashMap;

// based on: https://github.com/gatling/gatling-maven-plugin-demo-java/blob/main/src/test/java/computerdatabase/ComputerDatabaseSimulation.java#L5
public class CachedLoadSimulation extends Simulation {

	private final static Map<String, String> sentHeaders = new HashMap<>();
	static {
		sentHeaders.put("accept", "application/json");
	}
	private final static String baseUrl = "http://localhost:8080";
	ChainBuilder upload =
			// retry 5 times
			tryMax(5).on(exec(http("Get")
					// "/springboot/getCachedUser" + "?id=10"
					.get("/springboot/getUser" + "?id=10").headers(sentHeaders)
					.check(status().is(session -> 200)))).exitHereIfFailed();

	HttpProtocolBuilder httpProtocol = http.baseUrl(baseUrl)
			.acceptHeader(
					"text/html,application/xhtml+xml,application/xml,application/json")
			.acceptLanguageHeader("en-US,en;q=0.5")
			.acceptEncodingHeader("gzip, deflate");

	ScenarioBuilder agents = scenario("Agents").exec(upload);

	{
		setUp(agents.injectOpen(rampUsers(1000).during(10)))
				.protocols(httpProtocol);
	}
}

