package example.service;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import javax.annotation.Resource;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import example.repository.AxixsRepository;
import example.projection.ServerInstanceApplication;
import example.utils.HostData;
import example.projection.ServerInstanceApplication;
import io.prometheus.client.Collector.MetricFamilySamples;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Counter;
import io.prometheus.client.Gauge;
import io.prometheus.client.Gauge.Builder;
import io.prometheus.client.Histogram;
import io.prometheus.client.exporter.common.TextFormat;

@Service
public class NodeExporter {
	private static final Logger logger = LogManager.getLogger(NodeExporter.class);

	@Autowired
	private AxixsRepository dao;

	// custom metric setting the instance
	// https://prometheus.github.io/client_java/io/prometheus/client/Gauge.html
	private static final boolean debug = true;

	String fileName = "cluster.yaml";
	Map<String, ServerInstanceApplication> hostInfo = new HashMap<>();

	private static Map<String, Gauge> gauges = new HashMap<>();

	private CollectorRegistry registry;
	private Gauge example = null;
	private HostData hostData = null;
	private Map<String, String> data = new HashMap<>();
	private Map<String, String> metricTaker = new HashMap<>(); // currently unused

	private static final List<String> metricNames = Arrays.asList("memory", "cpu",
			"disk", "rpm");

	private void createGauge(String counterName) {
		// cache the gauge objects
		if (gauges.containsKey(counterName))
			return;
		// NOTE: check potential name collisions before register
		Enumeration<MetricFamilySamples> metricFamilySamplesEnumeration = registry
				.metricFamilySamples();
		List<String> definedMetricNames = new ArrayList<>();
		while (metricFamilySamplesEnumeration.hasMoreElements()) {
			MetricFamilySamples metricFamilySamples = metricFamilySamplesEnumeration
					.nextElement();
			// NOTE: getNames() was not available in 0.11.0 or earlier releases
			String[] names = metricFamilySamples.getNames();
			definedMetricNames.addAll(Arrays.asList(names));
		}

		if (debug)
			logger.info("Defined metric family sample names:" + definedMetricNames);

		try {
			if (!definedMetricNames.contains(counterName)) {
				Builder builder = Gauge
						.build(counterName, "Value of metric from instance")
						.labelNames(new String[] { "instance", "dc", "app", "env" });
				example = builder.register(registry);
				gauges.put(counterName, example);
			}
		} catch (Exception e) {
			logger.error("skipping metric update - exception: " + e.getMessage());
			// e.printStackTrace();
		}
	}

	private void exampleGauge(String counterName, ServerInstanceApplication host,
			float value) {

		String hostname = host.getServerName();
		String datacenter = "dummy";
		String application = host.getApplicationName();
		String env = host.getInstanceName();
		Gauge gauge = gauges.get(counterName);
		// invoke Prometheus variadic methods with a argument array set at
		// compile-time
		// can also pass the arguments explicitly as in
		// gauge.labels(hostname,datacenter,appid,environment).set(value);
		// String[] labelArgs = new String[] {};
		// java.lang.ArrayIndexOutOfBoundsException
		String[] labelArgs = new String[4];
		labelArgs[0] = hostname;
		labelArgs[1] = datacenter;
		labelArgs[2] = application;
		labelArgs[3] = env;

		// https://stackoverflow.com/questions/12320429/java-how-to-check-the-type-of-an-arraylist-as-a-whole
		/*
		int index = 0;
		String element = "memory";
		if (metricNames instanceof ArrayList<?>)
			((ArrayList<String>) metricNames).set(index, element);
		*/
		gauge.labels(labelArgs).set(value);
		if (debug)
			logger.info(String.format("Adding custom metrics %s %s %s %s %s: ",
					counterName, labelArgs) + gauge.labels(labelArgs).get());

	}

	private void exampleGauge(String counterName, String[] labels, float value) {

		Gauge gauge = gauges.get(counterName);

		if (debug)
			logger.info(String.format("Adding custom metrics %s %s: ", counterName,
					Arrays.asList(labels)));
		gauge.labels(labels).set(value);
		if (debug)
			logger.info(String.format("Added custom metrics %s %s: ", counterName,
					Arrays.asList(labels)) + gauge.labels(labels).get());

	}

	public String metrics() {

		logger.info("Starting reporting metrics");
		Writer writer = new StringWriter();
		try {
			registry = CollectorRegistry.defaultRegistry;

			metricTaker.put("load_average",
					"\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(\\S+)\\s*");

			List<Object[]> payload = dao.findAllData();
			for (Object[] row : payload) {

				String hostname = row[0].toString();
				hostData = new HostData(hostname);
				hostData.setMetrics(metricNames);
				hostData.setMetricTaker(metricTaker);
				hostData.readData();
				data = hostData.getData();
				if (data != null && !data.isEmpty()) {
					// copyOf
					String[] labels = Arrays.copyOfRange(row, 0, row.length,
							String[].class);
					if (debug)
						logger.info(String.format(
								"Loading inventory %d metrics for host: %s labels %s",
								data.keySet().size(), hostname, Arrays.asList(row)));

					for (String metricName : data.keySet()) {
						createGauge(metricName);
						exampleGauge(metricName, labels,
								Float.parseFloat(data.get(metricName)));
					}
				} else {
					if (debug)
						logger.info(String.format("No metrics for host: %s", hostname));
				}
			}
			TextFormat.write004(writer, registry.metricFamilySamples());
		} catch (

		IOException e) {
			logger.error("Exception (caught):" + e.toString());
			return null;
		}
		return writer.toString();
	}
}
