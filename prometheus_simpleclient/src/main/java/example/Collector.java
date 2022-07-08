
package example;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

import example.exemplars.Exemplar;

public abstract class Collector {

	public abstract List<MetricFamilySamples> collect();

	public List<MetricFamilySamples> collect(Predicate<String> sampleNameFilter) {
		List<MetricFamilySamples> all = collect();
		if (sampleNameFilter == null) {
			return all;
		}
		List<MetricFamilySamples> remaining = new ArrayList<MetricFamilySamples>(
				all.size());
		for (MetricFamilySamples mfs : all) {
			for (String name : mfs.getNames()) {
				if (sampleNameFilter.test(name)) {
					remaining.add(mfs);
					break;
				}
			}
		}
		return remaining;
	}

	public enum Type {
		UNKNOWN, // This is untyped in Prometheus text format.
		COUNTER, GAUGE, STATE_SET, INFO, HISTOGRAM, GAUGE_HISTOGRAM, SUMMARY,
	}

	static public class MetricFamilySamples {
		public final String name;
		public final String unit;
		public final Type type;
		public final String help;
		public final List<Sample> samples; // this list is modified when samples are
																				// added/removed.

		public MetricFamilySamples(String name, Type type, String help,
				List<Sample> samples) {
			this(name, "", type, help, samples);
		}

		public MetricFamilySamples(String name, String unit, Type type, String help,
				List<Sample> samples) {
			if (!unit.isEmpty() && !name.endsWith("_" + unit)) {
				throw new IllegalArgumentException(
						"Metric's unit is not the suffix of the metric name: " + name);
			}
			if ((type == Type.INFO || type == Type.STATE_SET) && !unit.isEmpty()) {
				throw new IllegalArgumentException(
						"Metric is of a type that cannot have a unit: " + name);
			}
			List<Sample> mungedSamples = samples;
			// Deal with _total from pre-OM automatically.
			if (type == Type.COUNTER) {
				if (name.endsWith("_total")) {
					name = name.substring(0, name.length() - 6);
				}
				String withTotal = name + "_total";
				mungedSamples = new ArrayList<Sample>(samples.size());
				for (Sample s : samples) {
					String n = s.name;
					if (name.equals(n)) {
						n = withTotal;
					}
					mungedSamples.add(new Sample(n, s.labelNames, s.labelValues, s.value,
							s.exemplar, s.timestampMs));
				}
			}
			this.name = name;
			this.unit = unit;
			this.type = type;
			this.help = help;
			this.samples = mungedSamples;
		}

		public MetricFamilySamples filter(Predicate<String> sampleNameFilter) {
			if (sampleNameFilter == null) {
				return this;
			}
			List<Sample> remainingSamples = new ArrayList<Sample>(samples.size());
			for (Sample sample : samples) {
				if (sampleNameFilter.test(sample.name)) {
					remainingSamples.add(sample);
				}
			}
			if (remainingSamples.isEmpty()) {
				return null;
			}
			return new MetricFamilySamples(name, unit, type, help, remainingSamples);
		}

		public String[] getNames() {
			switch (type) {
			case COUNTER:
				return new String[] { name + "_total", name + "_created", name };
			case SUMMARY:
				return new String[] { name + "_count", name + "_sum", name + "_created",
						name };
			case HISTOGRAM:
				return new String[] { name + "_count", name + "_sum", name + "_bucket",
						name + "_created", name };
			case GAUGE_HISTOGRAM:
				return new String[] { name + "_gcount", name + "_gsum",
						name + "_bucket", name };
			case INFO:
				return new String[] { name + "_info", name };
			default:
				return new String[] { name };
			}
		}

		@Override
		public boolean equals(Object obj) {
			if (!(obj instanceof MetricFamilySamples)) {
				return false;
			}
			MetricFamilySamples other = (MetricFamilySamples) obj;

			return other.name.equals(name) && other.unit.equals(unit)
					&& other.type.equals(type) && other.help.equals(help)
					&& other.samples.equals(samples);
		}

		@Override
		public int hashCode() {
			int hash = 1;
			hash = 37 * hash + name.hashCode();
			hash = 37 * hash + unit.hashCode();
			hash = 37 * hash + type.hashCode();
			hash = 37 * hash + help.hashCode();
			hash = 37 * hash + samples.hashCode();
			return hash;
		}

		@Override
		public String toString() {
			return "Name: " + name + " Unit:" + unit + " Type: " + type + " Help: "
					+ help + " Samples: " + samples;
		}

			public static class Sample {
			public final String name;
			public final List<String> labelNames;
			public final List<String> labelValues; // Must have same length as
																							// labelNames.
			public final double value;
			public final Exemplar exemplar;
			public final Long timestampMs; // It's an epoch format with milliseconds
																			// value included (this field is subject
																			// to change).

			public Sample(String name, List<String> labelNames,
					List<String> labelValues, double value, Exemplar exemplar,
					Long timestampMs) {
				this.name = name;
				this.labelNames = labelNames;
				this.labelValues = labelValues;
				this.value = value;
				this.exemplar = exemplar;
				this.timestampMs = timestampMs;
			}

			public Sample(String name, List<String> labelNames,
					List<String> labelValues, double value, Long timestampMs) {
				this(name, labelNames, labelValues, value, null, timestampMs);
			}

			public Sample(String name, List<String> labelNames,
					List<String> labelValues, double value, Exemplar exemplar) {
				this(name, labelNames, labelValues, value, exemplar, null);
			}

			public Sample(String name, List<String> labelNames,
					List<String> labelValues, double value) {
				this(name, labelNames, labelValues, value, null, null);
			}

			@Override
			public boolean equals(Object obj) {
				if (!(obj instanceof Sample)) {
					return false;
				}
				Sample other = (Sample) obj;

				return other.name.equals(name) && other.labelNames.equals(labelNames)
						&& other.labelValues.equals(labelValues) && other.value == value
						&& (exemplar == null && other.exemplar == null
								|| other.exemplar != null && other.exemplar.equals(exemplar))
						&& (timestampMs == null && other.timestampMs == null
								|| other.timestampMs != null
										&& other.timestampMs.equals(timestampMs));
			}

			@Override
			public int hashCode() {
				int hash = 1;
				hash = 37 * hash + name.hashCode();
				hash = 37 * hash + labelNames.hashCode();
				hash = 37 * hash + labelValues.hashCode();
				long d = Double.doubleToLongBits(value);
				hash = 37 * hash + (int) (d ^ (d >>> 32));
				if (timestampMs != null) {
					hash = 37 * hash + timestampMs.hashCode();
				}
				if (exemplar != null) {
					hash = 37 * exemplar.hashCode();
				}
				return hash;
			}

			@Override
			public String toString() {
				return "Name: " + name + " LabelNames: " + labelNames + " labelValues: "
						+ labelValues + " Value: " + value + " TimestampMs: " + timestampMs;
			}
		}
	}

	/**
	 * Register the Collector with the default registry.
	 */
	public <T extends Collector> T register() {
		return register(CollectorRegistry.defaultRegistry);
	}

	/**
	 * Register the Collector with the given registry.
	 */
	public <T extends Collector> T register(CollectorRegistry registry) {
		registry.register(this);
		return (T) this;
	}

	public interface Describable {
		List<MetricFamilySamples> describe();
	}

	public static final double NANOSECONDS_PER_SECOND = 1E9;
	public static final double MILLISECONDS_PER_SECOND = 1E3;

	private static final Pattern METRIC_NAME_RE = Pattern
			.compile("[a-zA-Z_:][a-zA-Z0-9_:]*");
	private static final Pattern METRIC_LABEL_NAME_RE = Pattern
			.compile("[a-zA-Z_][a-zA-Z0-9_]*");
	private static final Pattern RESERVED_METRIC_LABEL_NAME_RE = Pattern
			.compile("__.*");

	protected static void checkMetricName(String name) {
		if (!METRIC_NAME_RE.matcher(name).matches()) {
			throw new IllegalArgumentException("Invalid metric name: " + name);
		}
	}

	private static final Pattern SANITIZE_PREFIX_PATTERN = Pattern
			.compile("^[^a-zA-Z_:]");
	private static final Pattern SANITIZE_BODY_PATTERN = Pattern
			.compile("[^a-zA-Z0-9_:]");

	/**
	 * Sanitize metric name
	 */
	public static String sanitizeMetricName(String metricName) {
		return SANITIZE_BODY_PATTERN
				.matcher(SANITIZE_PREFIX_PATTERN.matcher(metricName).replaceFirst("_"))
				.replaceAll("_");
	}

	/**
	 * Throw an exception if the metric label name is invalid.
	 */
	protected static void checkMetricLabelName(String name) {
		if (!METRIC_LABEL_NAME_RE.matcher(name).matches()) {
			throw new IllegalArgumentException("Invalid metric label name: " + name);
		}
		if (RESERVED_METRIC_LABEL_NAME_RE.matcher(name).matches()) {
			throw new IllegalArgumentException(
					"Invalid metric label name, reserved for internal use: " + name);
		}
	}

	public static String doubleToGoString(double d) {
		if (d == Double.POSITIVE_INFINITY) {
			return "+Inf";
		}
		if (d == Double.NEGATIVE_INFINITY) {
			return "-Inf";
		}
		return Double.toString(d);
	}
}
