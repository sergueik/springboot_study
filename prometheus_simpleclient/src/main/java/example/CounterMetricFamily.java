package example;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class CounterMetricFamily extends Collector.MetricFamilySamples {

	private final List<String> labelNames;

	public CounterMetricFamily(String name, String help, double value) {
		super(name, Collector.Type.COUNTER, help, new ArrayList<Sample>());
		labelNames = Collections.emptyList();
		samples.add(new Sample(this.name + "_total", labelNames,
				Collections.<String> emptyList(), value));
	}

	public CounterMetricFamily(String name, String help,
			List<String> labelNames) {
		super(name, Collector.Type.COUNTER, help, new ArrayList<Sample>());
		this.labelNames = labelNames;
	}

	public CounterMetricFamily addMetric(List<String> labelValues, double value) {
		if (labelValues.size() != labelNames.size()) {
			throw new IllegalArgumentException("Incorrect number of labels.");
		}
		samples.add(new Sample(name + "_total", labelNames, labelValues, value));
		return this;
	}
}
