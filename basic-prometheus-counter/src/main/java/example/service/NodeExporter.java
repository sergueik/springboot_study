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

import example.dao.JDBCDao;
import example.entity.Host;
import example.entity.Result;
import example.service.BaseService;
import example.utils.ClusterConfigReader;
import example.utils.HostData;
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

	private ClusterConfigReader clusterConfigReader = new ClusterConfigReader();

	@Autowired
	private JDBCDao dao;

	// custom metric setting the instance
	// https://prometheus.github.io/client_java/io/prometheus/client/Gauge.html
	private static final boolean debug = false;

	@Resource
	private BaseService service;

	String fileName = "cluster.yaml";
	Map<String, Host> hostInfo = new HashMap<>();

	private static Map<String, Gauge> gauges = new HashMap<>();

	private CollectorRegistry registry;
	private Gauge example = null;
	private HostData hostData = null;
	private Map<String, String> data = new HashMap<>();
	private Map<String, String> metricTaker = new HashMap<>(); // currently unused

	private static final List<String> metricNames = Arrays.asList("memory", "cpu",
			"disk", "rpm");
	// private List<String> metricNames = new ArrayList<>();
	private static Random random = new Random();
	private static float value = 42;
	private static final int length = 10;

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
						.labelNames(new String[] { "instance", "datacenter", "appid",
								"environment" });
				example = builder.register(registry);
				gauges.put(counterName, example);
			}
		} catch (Exception e) {
			logger.error("skipping metric update - exception: " + e.getMessage());
			// e.printStackTrace();
		}
	}

	private void exampleGauge(String counterName, Host host, float value) {

		String hostname = host.getHostname();
		String datacenter = host.getDatacenter();
		String appid = host.getAppid();
		String environment = host.getEnvironment();
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
		labelArgs[2] = appid;
		labelArgs[3] = environment;

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

	public String metrics() {

		logger.info("Starting reporting metrics");
		Writer writer = new StringWriter();
		try {
			registry = CollectorRegistry.defaultRegistry;

			clusterConfigReader.read(fileName);
			hostInfo = clusterConfigReader.getInfo();
			metricTaker.put("load_average",
					"\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(?:\\S+)\\s\\s*(\\S+)\\s*");

			for (String hostname : hostInfo.keySet()) {
				// YAML
				hostData = new HostData(hostname);
				hostData.setMetrics(metricNames);
				hostData.setMetricTaker(metricTaker);
				hostData.readData();
				data = hostData.getData();
				// JDBC
				try {

					Result result = service.findHostByHostname(hostname);
					Object dataObj = result.getData();
					if (dataObj == null) {
						logger.info("No info in database  for host : " + hostname);
					} else {
						logger.info("Loading database info for host : " + dataObj);
						if (dataObj instanceof Host) {
							Map<String, String> hostData = new HashMap<>();
							hostData.put("hostname", ((Host) dataObj).getHostname());
							hostData.put("environment", ((Host) dataObj).getEnvironment());
							hostData.put("appid", ((Host) dataObj).getAppid());
							hostData.put("datacenter", ((Host) dataObj).getDatacenter());
							hostInfo.put(hostname, (Host) dataObj);
						}
					}
					// NOTE: org.sqlite.SQLiteException is not thrown to the code
					// attempt to catch produces Unreachable catch block compiler error
				} catch (Exception e) {
					logger.info(" Ignoring exception: " + e.getMessage());
				}

				for (String metricName : data.keySet()) {
					createGauge(metricName);
					// generate random metrics
					value = (hostname.matches(".*00$")) ? 42
							: Float
									.parseFloat(data.get(metricName) + random.nextInt((int) 42));

					exampleGauge(metricName, (Host) hostInfo.get(hostname), value);
				}
			}
			TextFormat.write004(writer, registry.metricFamilySamples());
		} catch (IOException e) {
			logger.error("Exception (caught):" + e.toString());
			return null;
		}
		return writer.toString();
	}
}
