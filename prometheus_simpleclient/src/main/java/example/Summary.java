package example;

import java.io.Closeable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.SortedMap;
import java.util.TreeMap;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import example.CKMSQuantiles.Quantile;

public class Summary extends SimpleCollector<Summary.Child>
		implements Counter.Describable {

	final List<Quantile> quantiles; // Can be empty, but can never be null.
	final long maxAgeSeconds;
	final int ageBuckets;

	Summary(Builder b) {
		super(b);
		quantiles = Collections
				.unmodifiableList(new ArrayList<Quantile>(b.quantiles));
		this.maxAgeSeconds = b.maxAgeSeconds;
		this.ageBuckets = b.ageBuckets;
		initializeNoLabelsChild();
	}

	public static class Builder
			extends SimpleCollector.Builder<Builder, Summary> {

		private final List<Quantile> quantiles = new ArrayList<Quantile>();
		private long maxAgeSeconds = TimeUnit.MINUTES.toSeconds(10);
		private int ageBuckets = 5;

		/**
		 * The class JavaDoc for {@link Summary} has more information on {@link #quantile(double, double)}.
		 * @see Summary
		 */
		public Builder quantile(double quantile, double error) {
			if (quantile < 0.0 || quantile > 1.0) {
				throw new IllegalArgumentException("Quantile " + quantile
						+ " invalid: Expected number between 0.0 and 1.0.");
			}
			if (error < 0.0 || error > 1.0) {
				throw new IllegalArgumentException("Error " + error
						+ " invalid: Expected number between 0.0 and 1.0.");
			}
			quantiles.add(new Quantile(quantile, error));
			return this;
		}

		/**
		 * The class JavaDoc for {@link Summary} has more information on {@link #maxAgeSeconds(long)} 
		 * @see Summary
		 */
		public Builder maxAgeSeconds(long maxAgeSeconds) {
			if (maxAgeSeconds <= 0) {
				throw new IllegalArgumentException(
						"maxAgeSeconds cannot be " + maxAgeSeconds);
			}
			this.maxAgeSeconds = maxAgeSeconds;
			return this;
		}

		/**
		 * The class JavaDoc for {@link Summary} has more information on {@link #ageBuckets(int)} 
		 * @see Summary
		 */
		public Builder ageBuckets(int ageBuckets) {
			if (ageBuckets <= 0) {
				throw new IllegalArgumentException(
						"ageBuckets cannot be " + ageBuckets);
			}
			this.ageBuckets = ageBuckets;
			return this;
		}

		@Override
		public Summary create() {
			for (String label : labelNames) {
				if (label.equals("quantile")) {
					throw new IllegalStateException(
							"Summary cannot have a label named 'quantile'.");
				}
			}
			dontInitializeNoLabelsChild = true;
			return new Summary(this);
		}
	}

	/**
	 *  Return a Builder to allow configuration of a new Summary. Ensures required fields are provided.
	 *
	 *  @param name The name of the metric
	 *  @param help The help string of the metric
	 */
	public static Builder build(String name, String help) {
		return new Builder().name(name).help(help);
	}

	/**
	 *  Return a Builder to allow configuration of a new Summary.
	 */
	public static Builder build() {
		return new Builder();
	}

	@Override
	protected Child newChild() {
		return new Child(quantiles, maxAgeSeconds, ageBuckets);
	}

	/**
	 * Represents an event being timed.
	 */
	public static class Timer implements Closeable {
		private final Child child;
		private final long start;

		private Timer(Child child, long start) {
			this.child = child;
			this.start = start;
		}

		public double observeDuration() {
			double elapsed = SimpleTimer.elapsedSecondsFromNanos(start,
					SimpleTimer.defaultTimeProvider.nanoTime());
			child.observe(elapsed);
			return elapsed;
		}

		/**
		 * Equivalent to calling {@link #observeDuration()}.
		 */
		@Override
		public void close() {
			observeDuration();
		}
	}

	public static class Child {

		public double time(Runnable timeable) {
			Timer timer = startTimer();

			double elapsed;
			try {
				timeable.run();
			} finally {
				elapsed = timer.observeDuration();
			}
			return elapsed;
		}

		public <E> E time(Callable<E> timeable) {
			Timer timer = startTimer();

			try {
				return timeable.call();
			} catch (RuntimeException e) {
				throw e;
			} catch (Exception e) {
				throw new RuntimeException(e);
			} finally {
				timer.observeDuration();
			}
		}

		public static class Value {
			public final double count;
			public final double sum;
			public final SortedMap<Double, Double> quantiles;
			public final long created;

			private Value(double count, double sum, List<Quantile> quantiles,
					TimeWindowQuantiles quantileValues, long created) {
				this.count = count;
				this.sum = sum;
				this.quantiles = Collections
						.unmodifiableSortedMap(snapshot(quantiles, quantileValues));
				this.created = created;
			}

			private SortedMap<Double, Double> snapshot(List<Quantile> quantiles,
					TimeWindowQuantiles quantileValues) {
				SortedMap<Double, Double> result = new TreeMap<Double, Double>();
				for (Quantile q : quantiles) {
					result.put(q.quantile, quantileValues.get(q.quantile));
				}
				return result;
			}
		}

		// Having these separate leaves us open to races,
		// however Prometheus as whole has other races
		// that mean adding atomicity here wouldn't be useful.
		// This should be reevaluated in the future.
		private final DoubleAdder count = new DoubleAdder();
		private final DoubleAdder sum = new DoubleAdder();
		private final List<Quantile> quantiles;
		private final TimeWindowQuantiles quantileValues;
		private final long created = System.currentTimeMillis();

		private Child(List<Quantile> quantiles, long maxAgeSeconds,
				int ageBuckets) {
			this.quantiles = quantiles;
			if (quantiles.size() > 0) {
				quantileValues = new TimeWindowQuantiles(
						quantiles.toArray(new Quantile[] {}), maxAgeSeconds, ageBuckets);
			} else {
				quantileValues = null;
			}
		}

		/**
		 * Observe the given amount.
		 * @param amt in most cases amt should be &gt;= 0. Negative values are supported, but you should read
		 *            <a href="https://prometheus.io/docs/practices/histograms/#count-and-sum-of-observations">
		 *            https://prometheus.io/docs/practices/histograms/#count-and-sum-of-observations</a> for
		 *            implications and alternatives.
		 */
		public void observe(double amt) {
			count.add(1);
			sum.add(amt);
			if (quantileValues != null) {
				quantileValues.insert(amt);
			}
		}

		/**
		 * Start a timer to track a duration.
		 * <p>
		 * Call {@link Timer#observeDuration} at the end of what you want to measure the duration of.
		 */
		public Timer startTimer() {
			return new Timer(this, SimpleTimer.defaultTimeProvider.nanoTime());
		}

		/**
		 * Get the value of the Summary.
		 * <p>
		 * <em>Warning:</em> The definition of {@link Value} is subject to change.
		 */
		public Value get() {
			return new Value(count.sum(), sum.sum(), quantiles, quantileValues,
					created);
		}
	}

	// Convenience methods.
	/**
	 * Observe the given amount on the summary with no labels.
	 * @param amt in most cases amt should be &gt;= 0. Negative values are supported, but you should read
	 *            <a href="https://prometheus.io/docs/practices/histograms/#count-and-sum-of-observations">
	 *            https://prometheus.io/docs/practices/histograms/#count-and-sum-of-observations</a> for
	 *            implications and alternatives.
	 */
	public void observe(double amt) {
		noLabelsChild.observe(amt);
	}

	/**
	 * Start a timer to track a duration on the summary with no labels.
	 * <p>
	 * Call {@link Timer#observeDuration} at the end of what you want to measure the duration of.
	 */
	public Timer startTimer() {
		return noLabelsChild.startTimer();
	}

	/**
	 * Executes runnable code (e.g. a Java 8 Lambda) and observes a duration of how long it took to run.
	 *
	 * @param timeable Code that is being timed
	 * @return Measured duration in seconds for timeable to complete.
	 */
	public double time(Runnable timeable) {
		return noLabelsChild.time(timeable);
	}

	/**
	 * Executes callable code (e.g. a Java 8 Lambda) and observes a duration of how long it took to run.
	 *
	 * @param timeable Code that is being timed
	 * @return Result returned by callable.
	 */
	public <E> E time(Callable<E> timeable) {
		return noLabelsChild.time(timeable);
	}

	/**
	 * Get the value of the Summary.
	 * <p>
	 * <em>Warning:</em> The definition of {@link Child.Value} is subject to change.
	 */
	public Child.Value get() {
		return noLabelsChild.get();
	}

	@Override
	public List<MetricFamilySamples> collect() {
		List<MetricFamilySamples.Sample> samples = new ArrayList<MetricFamilySamples.Sample>();
		for (Map.Entry<List<String>, Child> c : children.entrySet()) {
			Child.Value v = c.getValue().get();
			List<String> labelNamesWithQuantile = new ArrayList<String>(labelNames);
			labelNamesWithQuantile.add("quantile");
			for (Map.Entry<Double, Double> q : v.quantiles.entrySet()) {
				List<String> labelValuesWithQuantile = new ArrayList<String>(
						c.getKey());
				labelValuesWithQuantile.add(doubleToGoString(q.getKey()));
				samples.add(new MetricFamilySamples.Sample(fullname,
						labelNamesWithQuantile, labelValuesWithQuantile, q.getValue()));
			}
			samples.add(new MetricFamilySamples.Sample(fullname + "_count",
					labelNames, c.getKey(), v.count));
			samples.add(new MetricFamilySamples.Sample(fullname + "_sum", labelNames,
					c.getKey(), v.sum));
			samples.add(new MetricFamilySamples.Sample(fullname + "_created",
					labelNames, c.getKey(), v.created / 1000.0));
		}

		return familySamplesList(Type.SUMMARY, samples);
	}

	@Override
	public List<MetricFamilySamples> describe() {
		return Collections.<MetricFamilySamples> singletonList(
				new SummaryMetricFamily(fullname, help, labelNames));
	}

}
