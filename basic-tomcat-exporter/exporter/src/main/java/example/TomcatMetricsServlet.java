package example;

import javax.servlet.ServletConfig;
import javax.servlet.annotation.WebServlet;

import io.prometheus.client.Collector;
import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.exporter.MetricsServlet;
import io.prometheus.client.hotspot.DefaultExports;
import nl.nlighten.prometheus.tomcat.TomcatDbcp2PoolExports;
import nl.nlighten.prometheus.tomcat.TomcatGenericExports;
import nl.nlighten.prometheus.tomcat.TomcatJdbcPoolExports;

import java.lang.IllegalArgumentException;
import java.util.Arrays;
import java.util.Enumeration;
import java.util.HashSet;

@WebServlet("/")
public class TomcatMetricsServlet extends MetricsServlet {

	@Override
	public void init(ServletConfig config) {
		if (!initialized()) {
			// java.lang.IllegalArgumentException: Failed to register Collector of
			// type ClassLoadingExports: The Collector exposes the same name multiple
			// times: jvm_classes_loaded
			try {
				DefaultExports.initialize();
			} catch (IllegalArgumentException e) {
				// ignore
			}
			new TomcatGenericExports(false).register();
			if (TomcatJdbcPoolExports.isTomcatJdbcUsed()) {
				new TomcatJdbcPoolExports().register();
			} else {
				new TomcatDbcp2PoolExports().register();
			}
		}
	}

	private boolean initialized() {
		Enumeration<Collector.MetricFamilySamples> samples = CollectorRegistry.defaultRegistry
				.filteredMetricFamilySamples(
						new HashSet<String>(Arrays.asList("tomcat_info")));
		return samples.hasMoreElements();
	}
}

