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
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import example.repository.AxixsRepository;
import example.projection.ServerInstanceApplication;
import example.utils.HostData;

import io.prometheus.client.Collector.MetricFamilySamples;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Collector.MetricFamilySamples.Sample;
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

	Map<String, ServerInstanceApplication> hostInfo = new HashMap<>();

	private static Map<String, Gauge> gauges = new HashMap<>();

	private CollectorRegistry registry;
	private Gauge example = null;
	private HostData hostData = null;
	private Map<String, String> data = new HashMap<>();

	// https://stackoverflow.com/questions/26275736/how-to-pass-a-mapstring-string-with-application-properties
	@Value("#{${example.metricExtractors}}")
	private Map<String, String> metricExtractors;

	// https://stackoverflow.com/questions/6212898/spring-properties-file-get-element-as-an-array
	@Value("${example.labelNames}")
	private String[] labelNames;

	@Value("#{'${example.metricNames}'.split(',')}")
	private String[] metricNames;

	@Value("#{${example.extractedMetricNames}}")
	private Map<String, String> extractedMetricNames;

	private void createGauge(String counterName) {
		createGauge(counterName, labelNames);
	}

	// NOTE: keep the code - it does not work the intended way, unable to create
	// Gauge with same name as existing but with shorter array of labels
	private void createGauge(String counterName, String[] labels) {

		// cache the gauge objects
		if (gauges.containsKey(counterName))
			return;
		// NOTE: check potential name collisions before register
		// https://prometheus.github.io/client_java/io/prometheus/client/CollectorRegistry.html

		Enumeration<MetricFamilySamples> metricFamilySamplesEnumeration = registry
				.metricFamilySamples();
		List<String> definedMetricNames = new ArrayList<>();
		while (metricFamilySamplesEnumeration.hasMoreElements()) {
			MetricFamilySamples metricFamilySamples = metricFamilySamplesEnumeration
					.nextElement();
			// NOTE: getNames() was not available in 0.11.0 or earlier releases
			String[] names = metricFamilySamples.getNames();

			// NOTE: samples was not available in 0.11.0 or earlier releases
			// NOTE: accessing the property directly
			for (Sample sample : metricFamilySamples.samples) {
				// NOTE: accessing the property directly
				List<String> labelNames = sample.labelNames;
				if (debug)
					logger.info("Known metric " + sample.name + " label names: "
							+ Arrays.asList(labelNames));

			}

			definedMetricNames.addAll(Arrays.asList(names));
		}

		if (debug)
			logger.info(
					"Read already defined metric family names:" + definedMetricNames);

		try {
			// TODO: implement different number of labels
			// for earlier defined metric name
			if (!definedMetricNames.contains(counterName)) {
				Builder builder = Gauge.build(counterName,
						String.format("Value of metric from instance with labels %s",
								Arrays.asList(labels)))
						.labelNames(labels);

				if (debug)
					logger.info("Defined metric labels names: " + Arrays.asList(labels));

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
		labelArgs[2] = (application != null) ? application : "";
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

		if (labels[2] == null /* blank application */)
			labels[2] = "";
		Gauge gauge = gauges.get(counterName);

		if (debug)
			logger.info(String.format("Adding custom metrics %s %s: ", counterName,
					Arrays.asList(labels)));
		gauge.labels(labels).set(value);
		if (debug)
			logger.info(String.format("Added custom metrics %s %s: ", counterName,
					Arrays.asList(labels)) + gauge.labels(labels).get());

	}

	public String metricsFromServerInstanceList() {

		logger.info("Starting reporting metrics");
		Writer writer = new StringWriter();
		try {
			registry = CollectorRegistry.defaultRegistry;

			List<ServerInstanceApplication> payload = dao
					.findAllServerInstanceApplications();
			for (Object row : payload) {
				ServerInstanceApplication serverInstance = (ServerInstanceApplication) row;
				String hostname = serverInstance.getServerName();
				hostData = new HostData(hostname);
				hostData.setMetrics(Arrays.asList(metricNames));
				hostData.setMetricExtractors(metricExtractors);
				hostData.setExtractedMetricNames(extractedMetricNames);
				hostData.readData();
				data = hostData.getData();
				if (data != null && !data.isEmpty()) {
					if (debug)
						logger.info(
								String.format("Loading inventory %d metrics info for host: %s",
										data.keySet().size(), hostname));

					for (String metricName : data.keySet()) {
						createGauge(metricName);
						// create separate gauge for blank app label
						createGauge(metricName, new String[] { "instance", "dc", "env" });

						exampleGauge(metricName, serverInstance,
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

	public String metricsFromData() {

		logger.info("Starting reporting metrics");
		Writer writer = new StringWriter();
		try {
			registry = CollectorRegistry.defaultRegistry;

			List<Object[]> payload = dao.findAllData();
			for (Object[] row : payload) {

				String hostname = row[0].toString();
				hostData = new HostData(hostname);
				hostData.setExtractedMetricNames(extractedMetricNames);
				hostData.setMetrics(Arrays.asList(metricNames));
				// NOTE: cannot dynamically update the value anotated via @Value
				// it i java.util.Collections$UnmodifiableMap
				hostData.setMetricExtractors(metricExtractors);
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
						// create separate gauge for blank app label -
						// currently it will cease to create new metric in the registry
						// keep for the future use
						// createGauge(metricName, new String[] { "instance", "dc", "env"
						// });
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
