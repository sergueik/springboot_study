package example;

import java.util.ArrayList;
import java.util.List;
import java.util.Collections;

public class SummaryMetricFamily extends Collector.MetricFamilySamples {

	private final List<String> labelNames;
	private final List<Double> quantiles;

	public SummaryMetricFamily(String name, String help, double count,
			double sum) {
		super(name, Collector.Type.SUMMARY, help, new ArrayList<Sample>());
		this.labelNames = Collections.emptyList();
		this.quantiles = Collections.emptyList();
		addMetric(Collections.<String> emptyList(), count, sum);
	}

	public SummaryMetricFamily(String name, String help,
			List<String> labelNames) {
		this(name, help, labelNames, Collections.<Double> emptyList());
	}

	public SummaryMetricFamily(String name, String help, List<String> labelNames,
			List<Double> quantiles) {
		super(name, Collector.Type.SUMMARY, help, new ArrayList<Sample>());
		this.labelNames = labelNames;
		this.quantiles = quantiles;
	}

	public SummaryMetricFamily addMetric(List<String> labelValues, double count,
			double sum) {
		return addMetric(labelValues, count, sum, Collections.<Double> emptyList());
	}

	public SummaryMetricFamily addMetric(List<String> labelValues, double count,
			double sum, List<Double> quantiles) {
		if (labelValues.size() != labelNames.size()) {
			throw new IllegalArgumentException("Incorrect number of labels.");
		}
		if (this.quantiles.size() != quantiles.size()) {
			throw new IllegalArgumentException("Incorrect number of quantiles.");
		}
		samples.add(new Sample(name + "_count", labelNames, labelValues, count));
		samples.add(new Sample(name + "_sum", labelNames, labelValues, sum));
		List<String> labelNamesWithQuantile = new ArrayList<String>(labelNames);
		labelNamesWithQuantile.add("quantile");
		for (int i = 0; i < quantiles.size(); i++) {
			List<String> labelValuesWithQuantile = new ArrayList<String>(labelValues);
			labelValuesWithQuantile
					.add(Collector.doubleToGoString(this.quantiles.get(i)));
			samples.add(new Sample(name, labelNamesWithQuantile,
					labelValuesWithQuantile, quantiles.get(i)));
		}
		return this;
	}
}
