package computerdatabase;

import static io.gatling.javaapi.core.CoreDsl.*;
import static io.gatling.javaapi.http.HttpDsl.*;

import io.gatling.javaapi.core.*;
import io.gatling.javaapi.http.*;

import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

import java.util.Map;
import java.util.HashMap;

public class ComputerDatabaseSimulation extends Simulation {

	FeederBuilder<String> feeder = csv("search.csv").random();
	// https://gatling.io/docs/gatling/reference/current/http/request/s

	// Extracting a map of headers allows you to reuse these in several requests
	private final static Map<String, String> sentHeaders = new HashMap<>();
	static {
		sentHeaders.put("content-type",
				"multipart/form-data;boundary=\"boundary\"");
		sentHeaders.put("accept", "text/html");
	}
	private final static String data = "test data";

	//@formatter:off
	private static String body = String.join("\r\n",
			Arrays.asList(
					"--boundary",
					"Content-Disposition: form-data; name=\"file\"; filename=\"temp.txt\"",
					"Content-Type: application/octet-stream", 
					"", 
					data, 
					"",
					"--boundary--", 
					""));
	//@formatter:on

	ChainBuilder edit =
			// try 100 times
			tryMax(100).on(exec(
					http("Post").post("/basic/upload" + "?operation=send&param=something")
							// .headers(sentHeaders)
							.header("content-type",
									"multipart/form-data; boundary=\"boundary\"")
							.formParam("operation", "send").formParam("param", "something")
							.formParam("file", "temp.txt").body(StringBody(body))
							.check(status().is(session -> 200))))
					.exitHereIfFailed();

	HttpProtocolBuilder httpProtocol = http.baseUrl("http://localhost:8085")
			.acceptHeader("text/html,application/xhtml+xml,application/xml")
			.acceptLanguageHeader("en-US,en;q=0.5")
			.acceptEncodingHeader("gzip, deflate").userAgentHeader(
					"Mozilla/5.0 (Macintosh; Intel Mac OS X 10.8; rv:16.0) Gecko/20100101 Firefox/16.0");

	ScenarioBuilder admins = scenario("Admins").exec(edit);

	{
		setUp(admins.injectOpen(rampUsers(100).during(10))).protocols(httpProtocol);
	}
}
