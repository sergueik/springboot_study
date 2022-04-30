package example.controller;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.web.bind.annotation.RestController;

import example.utils.Host;
import example.utils.HostData;
import example.utils.ClusterConfigReader;

import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;

import io.prometheus.client.Collector.MetricFamilySamples;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Counter;
import io.prometheus.client.Gauge;
import io.prometheus.client.Gauge.Builder;
import io.prometheus.client.Histogram;
import io.prometheus.client.exporter.common.TextFormat;

// import io.micrometer.core.annotation.Timed;
// import io.micrometer.core.instrument.Counter;
// import io.micrometer.core.instrument.MeterRegistry;

import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.io.IOException;
import java.io.StringWriter;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.Level;

@RestController
@RequestMapping("/")
public class AppController {
	private static final Logger logger = LogManager
			.getLogger(AppController.class);

	private ClusterConfigReader snakeYamlReader = new ClusterConfigReader();

	// custom metric setting the instance
	// https://prometheus.github.io/client_java/io/prometheus/client/Gauge.html
	private static final boolean debug = false;
	String fileName = "cluster.yaml";
	Map<String, Host> hostInfo = new HashMap<>();

	private static Map<String, Gauge> metrics = new HashMap<>();

	private CollectorRegistry registry;
	private Gauge example = null;
	static final Counter requestsTotal = Counter.build().name("requests_total")
			.help("Total number of requests.").register();
	static final Histogram requestTimet = Histogram.build()
			.name("requests_latency_seconds").help("Request latency in seconds.")
			.register();
	private HostData hostData = null;
	private Map<String, String> data = new HashMap<>();
	private Map<String, String> metricTaker = new HashMap<>(); // currently unused

	private static final List<String> metricNames = Arrays.asList("memory",
			"load_average", "cpu", "disk", "rpm");
	// private List<String> metricNames = new ArrayList<>();
	private static Random random = new Random();
	private static float value = 42;
	private static final int length = 10;

	private static final Counter buildStatus = Counter.build()
			.name("build_status_counter").labelNames("status")
			.help("A simple Counter to illustrate custom build status and Prometheus")
			.register();

	@RequestMapping("/build")
	public void endpoint() {
		if (random.nextInt(2) > 0) {
			logger.log(org.apache.logging.log4j.Level.INFO,
					"incremented successful build counter");
			buildStatus.labels("success").inc();
		} else {
			logger.log(org.apache.logging.log4j.Level.INFO,
					"incremented failed build counter");
			buildStatus.labels("error").inc();
		}
	}

	@RequestMapping("hello")
	public String sayHello() {
		logger.info("increment requests_total");
		requestsTotal.inc();
		logger.info("creating the timer");
		Histogram.Timer requestTimer = requestTimet.startTimer();
		try {
			return "Hello World";
		} finally {
			logger.info("recording the requests_latency_seconds time duration");
			requestTimer.observeDuration();
		}
	}

	// application hosted metrics
	// see also:
	// https://www.tabnine.com/code/java/methods/io.prometheus.client.CollectorRegistry/metricFamilySamples
	@ResponseBody
	@GetMapping(value = "metrics", produces = MediaType.TEXT_PLAIN_VALUE)
	public ResponseEntity<String> metrics() {
		logger.info("Starting reporting metrics");
		Writer writer = new StringWriter();
		try {
			registry = CollectorRegistry.defaultRegistry;

			snakeYamlReader.read(fileName);
			hostInfo = snakeYamlReader.getInfo();

			for (String hostname : hostInfo.keySet()) {
				hostData = new HostData(hostname);
				hostData.setMetrics(metricNames);
				hostData.readData();
				data = hostData.getData();

				for (String metricName : data.keySet()) {
					createGauge(metricName);
					value = (hostname.matches(".*00$")) ? 42
							: Float.parseFloat(data.get(metricName));

					exampleGauge(metricName, (Host) hostInfo.get(hostname), value);
				}
			}
			TextFormat.write004(writer, registry.metricFamilySamples());
		} catch (IOException e) {
			logger.error("Exception (caught):" + e.toString());
			return ResponseEntity.status(HttpStatus.BAD_REQUEST).body(null);
		}
		return ResponseEntity.status(HttpStatus.OK).body(writer.toString());
	}

	private void createGauge(String counterName) {
		// cache the gauge objects
		if (metrics.containsKey(counterName))
			return;
		// NOTE: check potential name collisions before register
		Enumeration<MetricFamilySamples> metricFamilySamplesEnumeration = registry
				.metricFamilySamples();
		List<String> metricNames = new ArrayList<>();
		while (metricFamilySamplesEnumeration.hasMoreElements()) {
			MetricFamilySamples metricFamilySamples = metricFamilySamplesEnumeration
					.nextElement();
			// NOTE: getNames() was not available in 0.11.0 or earlier releases
			String[] names = metricFamilySamples.getNames();
			metricNames.addAll(Arrays.asList(names));
		}

		if (debug)
			logger.info("Metric Family Samples names:" + metricNames);

		try {
			if (!metricNames.contains((Object) counterName)) {
				Builder builder = Gauge
						.build(counterName, "Value of metric from instance").labelNames(
								new String[] { "instance", "domain", "app", "environment" });
				example = builder.register(registry);
				metrics.put(counterName, example);
			}
		} catch (Exception e) {
			logger.error("skipping metric update - exception: " + e.getMessage());
			// e.printStackTrace();
		}
	}

	// exampleGauge(counterName, instance + "00", 42);
	private void exampleGauge(String counterName, Host host, float value) {

		String hostname = host.getHostname();
		String domain = host.getDomain();
		String app = host.getApp();
		String environment = host.getEnvironment();
		Gauge gauge = metrics.get(counterName);
		// invoke Prometheus variadic methods with a argument array set at
		// compile-time
		// can also pass the arguments explicitly as in
		// gauge.labels(hostname,domain,app,environment).set(value);
		// String[] labelArgs = new String[] {};
		// java.lang.ArrayIndexOutOfBoundsException
		String[] labelArgs = new String[4];
		labelArgs[0] = hostname;
		labelArgs[1] = domain;
		labelArgs[2] = app;
		labelArgs[3] = environment;

		int index = 0;
		String element = "memory";
		/*
		if (metricNames instanceof ArrayList<?>)
			((ArrayList<String>) metricNames).set(index, element);
		*/
		gauge.labels(labelArgs).set(value);

		if (debug)
			logger.info(String.format("Adding custom metrics %s %s %s %s %s: ",
					counterName, labelArgs) + gauge.labels(labelArgs).get());

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
