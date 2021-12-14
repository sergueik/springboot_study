package example.controller;

import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Counter;
import io.prometheus.client.Histogram;
import io.prometheus.client.exporter.common.TextFormat;

import java.io.Writer;
import java.io.IOException;
import java.io.StringWriter;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@RestController
@RequestMapping("/")
public class AppController {
	private static final Logger logger = LoggerFactory
			.getLogger(AppController.class);

	static final Counter requestsTotal = Counter.build().name("requests_total")
			.help("Total number of requests.").register();
	static final Histogram requestTimet = Histogram.build()
			.name("requests_latency_seconds").help("Request latency in seconds.")
			.register();

	@RequestMapping("hello")
	public String sayHello() {
		logger.info("increment requests_total");
		requestsTotal.inc();
		logger.info("creating the time");
		Histogram.Timer requestTimer = requestTimet.startTimer();
		try {
			return "Hello World";
		} finally {
			logger.info("recording the requests_latency_seconds time duration");
			requestTimer.observeDuration();
		}
	}

	// application hosted metrics
	@ResponseBody
	@GetMapping(value = "metrics", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> metrics() {
		logger.info("Starting reporting metrics");
		Writer writer = new StringWriter();
		try {
			TextFormat.write004(writer,
					CollectorRegistry.defaultRegistry.metricFamilySamples());
		} catch (IOException e) {
			logger.error("Exception (caught):" + e.toString());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		return ResponseEntity.status(HttpStatus.OK).body(writer.toString());
	}

	// index page
	@GetMapping(produces = MediaType.TEXT_HTML_VALUE)
	@ResponseBody
	public ResponseEntity<String> index() {
		String body = "<html>" + "<head>" + "</head>" + "<body>"
				+ "<a href=\"./hello\"\">Main applicaion controller</a><br/>"
				+ "<a href=\"./metrics\"\">Application hosted metrics REST service</a><br/>"
				+ "<a href=\"./actuator/prometheus\"\">Default System  metrics for prometheus</a>"
				+ "</body>" + "</html>";
		return ResponseEntity.ok().body(body);
	}

}
