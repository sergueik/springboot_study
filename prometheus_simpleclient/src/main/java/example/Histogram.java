package example;

import java.io.Closeable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicReference;

import example.exemplars.Exemplar;
import example.exemplars.ExemplarConfig;
import example.exemplars.HistogramExemplarSampler;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;

public class Histogram extends SimpleCollector<Histogram.Child>
		implements Collector.Describable {
	private final double[] buckets;
	private final Boolean exemplarsEnabled; // null means default from
																					// ExemplarConfig applies
	private final HistogramExemplarSampler exemplarSampler;

	Histogram(Builder b) {
		super(b);
		this.exemplarsEnabled = b.exemplarsEnabled;
		this.exemplarSampler = b.exemplarSampler;
		buckets = b.buckets;
		initializeNoLabelsChild();
	}

	public static class Builder
			extends SimpleCollector.Builder<Builder, Histogram> {

		private Boolean exemplarsEnabled = null;
		private HistogramExemplarSampler exemplarSampler = null;
		private double[] buckets = new double[] { .005, .01, .025, .05, .075, .1,
				.25, .5, .75, 1, 2.5, 5, 7.5, 10 };

		@Override
		public Histogram create() {
			for (int i = 0; i < buckets.length - 1; i++) {
				if (buckets[i] >= buckets[i + 1]) {
					throw new IllegalStateException(
							"Histogram buckets must be in increasing order: " + buckets[i]
									+ " >= " + buckets[i + 1]);
				}
			}
			if (buckets.length == 0) {
				throw new IllegalStateException(
						"Histogram must have at least one bucket.");
			}
			for (String label : labelNames) {
				if (label.equals("le")) {
					throw new IllegalStateException(
							"Histogram cannot have a label named 'le'.");
				}
			}

			// Append infinity bucket if it's not already there.
			if (buckets[buckets.length - 1] != Double.POSITIVE_INFINITY) {
				double[] tmp = new double[buckets.length + 1];
				System.arraycopy(buckets, 0, tmp, 0, buckets.length);
				tmp[buckets.length] = Double.POSITIVE_INFINITY;
				buckets = tmp;
			}
			dontInitializeNoLabelsChild = true;
			return new Histogram(this);
		}

		public Builder buckets(double... buckets) {
			this.buckets = buckets;
			return this;
		}

		public Builder linearBuckets(double start, double width, int count) {
			buckets = new double[count];
			for (int i = 0; i < count; i++) {
				buckets[i] = start + i * width;
			}
			return this;
		}

		public Builder exponentialBuckets(double start, double factor, int count) {
			buckets = new double[count];
			for (int i = 0; i < count; i++) {
				buckets[i] = start * Math.pow(factor, i);
			}
			return this;
		}

		public Builder withExemplarSampler(
				HistogramExemplarSampler exemplarSampler) {
			if (exemplarSampler == null) {
				throw new NullPointerException();
			}
			this.exemplarSampler = exemplarSampler;
			return withExemplars();
		}

		public Builder withExemplars() {
			this.exemplarsEnabled = TRUE;
			return this;
		}

		public Builder withoutExemplars() {
			this.exemplarsEnabled = FALSE;
			return this;
		}
	}

	public static Builder build(String name, String help) {
		return new Builder().name(name).help(help);
	}

	public static Builder build() {
		return new Builder();
	}

	@Override
	protected Child newChild() {
		return new Child(buckets, exemplarsEnabled, exemplarSampler);
	}

	public static class Timer implements Closeable {
		private final Child child;
		private final long start;

		private Timer(Child child, long start) {
			this.child = child;
			this.start = start;
		}

		public double observeDuration() {
			return observeDurationWithExemplar((String[]) null);
		}

		public double observeDurationWithExemplar(String... exemplarLabels) {
			double elapsed = SimpleTimer.elapsedSecondsFromNanos(start,
					SimpleTimer.defaultTimeProvider.nanoTime());
			child.observeWithExemplar(elapsed, exemplarLabels);
			return elapsed;
		}

		public double observeDurationWithExemplar(
				Map<String, String> exemplarLabels) {
			return observeDurationWithExemplar(Exemplar.mapToArray(exemplarLabels));
		}

		@Override
		public void close() {
			observeDuration();
		}
	}

	public static class Child {

		public double time(Runnable timeable) {
			return timeWithExemplar(timeable, (String[]) null);
		}

		public double timeWithExemplar(Runnable timeable,
				String... exemplarLabels) {
			Timer timer = startTimer();

			double elapsed;
			try {
				timeable.run();
			} finally {
				elapsed = timer.observeDurationWithExemplar(exemplarLabels);
			}
			return elapsed;
		}

		public double timeWithExemplar(Runnable timeable,
				Map<String, String> exemplarLabels) {
			return timeWithExemplar(timeable, Exemplar.mapToArray(exemplarLabels));
		}

		public <E> E time(Callable<E> timeable) {
			return timeWithExemplar(timeable, (String[]) null);
		}

		public <E> E timeWithExemplar(Callable<E> timeable,
				String... exemplarLabels) {
			Timer timer = startTimer();

			try {
				return timeable.call();
			} catch (RuntimeException e) {
				throw e;
			} catch (Exception e) {
				throw new RuntimeException(e);
			} finally {
				timer.observeDurationWithExemplar(exemplarLabels);
			}
		}

		public <E> E timeWithExemplar(Callable<E> timeable,
				Map<String, String> exemplarLabels) {
			return timeWithExemplar(timeable, Exemplar.mapToArray(exemplarLabels));
		}

		public static class Value {
			public final double sum;
			public final double[] buckets;
			public final Exemplar[] exemplars;
			public final long created;

			public Value(double sum, double[] buckets, Exemplar[] exemplars,
					long created) {
				this.sum = sum;
				this.buckets = buckets;
				this.exemplars = exemplars;
				this.created = created;
			}
		}

		private Child(double[] buckets, Boolean exemplarsEnabled,
				HistogramExemplarSampler exemplarSampler) {
			upperBounds = buckets;
			this.exemplarsEnabled = exemplarsEnabled;
			this.exemplarSampler = exemplarSampler;
			exemplars = new ArrayList<AtomicReference<Exemplar>>(buckets.length);
			cumulativeCounts = new DoubleAdder[buckets.length];
			for (int i = 0; i < buckets.length; ++i) {
				cumulativeCounts[i] = new DoubleAdder();
				exemplars.add(new AtomicReference<Exemplar>());
			}
		}

		private final ArrayList<AtomicReference<Exemplar>> exemplars;
		private final Boolean exemplarsEnabled;
		private final HistogramExemplarSampler exemplarSampler;
		private final double[] upperBounds;
		private final DoubleAdder[] cumulativeCounts;
		private final DoubleAdder sum = new DoubleAdder();
		private final long created = System.currentTimeMillis();

		public void observe(double amt) {
			observeWithExemplar(amt, (String[]) null);
		}

		public void observeWithExemplar(double amt, String... exemplarLabels) {
			Exemplar exemplar = exemplarLabels == null ? null
					: new Exemplar(amt, System.currentTimeMillis(), exemplarLabels);
			for (int i = 0; i < upperBounds.length; ++i) {
				// The last bucket is +Inf, so we always increment.
				if (amt <= upperBounds[i]) {
					cumulativeCounts[i].add(1);
					updateExemplar(amt, i, exemplar);
					break;
				}
			}
			sum.add(amt);
		}

		public void observeWithExemplar(double amt,
				Map<String, String> exemplarLabels) {
			observeWithExemplar(amt, Exemplar.mapToArray(exemplarLabels));
		}

		private void updateExemplar(double amt, int i,
				Exemplar userProvidedExemplar) {
			AtomicReference<Exemplar> exemplar = exemplars.get(i);
			double bucketFrom = i == 0 ? Double.NEGATIVE_INFINITY
					: upperBounds[i - 1];
			double bucketTo = upperBounds[i];
			Exemplar prev, next;
			do {
				prev = exemplar.get();
				if (userProvidedExemplar != null) {
					next = userProvidedExemplar;
				} else {
					next = sampleNextExemplar(amt, bucketFrom, bucketTo, prev);
				}
				if (next == null || next == prev) {
					return;
				}
			} while (!exemplar.compareAndSet(prev, next));
		}

		private Exemplar sampleNextExemplar(double amt, double bucketFrom,
				double bucketTo, Exemplar prev) {
			if (FALSE.equals(exemplarsEnabled)) {
				return null;
			}
			if (exemplarSampler != null) {
				return exemplarSampler.sample(amt, bucketFrom, bucketTo, prev);
			}
			if (TRUE.equals(exemplarsEnabled)
					|| ExemplarConfig.isExemplarsEnabled()) {
				HistogramExemplarSampler exemplarSampler = ExemplarConfig
						.getHistogramExemplarSampler();
				if (exemplarSampler != null) {
					return exemplarSampler.sample(amt, bucketFrom, bucketTo, prev);
				}
			}
			return null;
		}

		public Timer startTimer() {
			return new Timer(this, SimpleTimer.defaultTimeProvider.nanoTime());
		}

		public Value get() {
			double[] buckets = new double[cumulativeCounts.length];
			Exemplar[] exemplars = new Exemplar[cumulativeCounts.length];
			double acc = 0;
			for (int i = 0; i < cumulativeCounts.length; ++i) {
				acc += cumulativeCounts[i].sum();
				buckets[i] = acc;
				exemplars[i] = this.exemplars.get(i).get();
			}
			return new Value(sum.sum(), buckets, exemplars, created);
		}
	}

	// Convenience methods.

	public void observe(double amt) {
		noLabelsChild.observe(amt);
	}

	public void observeWithExemplar(double amt, String... exemplarLabels) {
		noLabelsChild.observeWithExemplar(amt, exemplarLabels);
	}

	public void observeWithExemplar(double amt,
			Map<String, String> exemplarLabels) {
		noLabelsChild.observeWithExemplar(amt, exemplarLabels);
	}

	public Timer startTimer() {
		return noLabelsChild.startTimer();
	}

	public double time(Runnable timeable) {
		return noLabelsChild.time(timeable);
	}

	public double timeWithExemplar(Runnable timeable, String... exemplarLabels) {
		return noLabelsChild.timeWithExemplar(timeable, exemplarLabels);
	}

	public double timeWithExemplar(Runnable timeable,
			Map<String, String> exemplarLabels) {
		return noLabelsChild.timeWithExemplar(timeable, exemplarLabels);
	}

	public <E> E time(Callable<E> timeable) {
		return noLabelsChild.time(timeable);
	}

	public <E> E timeWithExemplar(Callable<E> timeable,
			String... exemplarLabels) {
		return noLabelsChild.timeWithExemplar(timeable, exemplarLabels);
	}

	public <E> E timeWithExemplar(Callable<E> timeable,
			Map<String, String> exemplarLabels) {
		return noLabelsChild.timeWithExemplar(timeable, exemplarLabels);
	}

	@Override
	public List<MetricFamilySamples> collect() {
		List<MetricFamilySamples.Sample> samples = new ArrayList<MetricFamilySamples.Sample>();
		for (Map.Entry<List<String>, Child> c : children.entrySet()) {
			Child.Value v = c.getValue().get();
			List<String> labelNamesWithLe = new ArrayList<String>(labelNames);
			labelNamesWithLe.add("le");
			for (int i = 0; i < v.buckets.length; ++i) {
				List<String> labelValuesWithLe = new ArrayList<String>(c.getKey());
				labelValuesWithLe.add(doubleToGoString(buckets[i]));
				samples.add(new MetricFamilySamples.Sample(fullname + "_bucket",
						labelNamesWithLe, labelValuesWithLe, v.buckets[i], v.exemplars[i]));
			}
			samples.add(new MetricFamilySamples.Sample(fullname + "_count",
					labelNames, c.getKey(), v.buckets[buckets.length - 1]));
			samples.add(new MetricFamilySamples.Sample(fullname + "_sum", labelNames,
					c.getKey(), v.sum));
			samples.add(new MetricFamilySamples.Sample(fullname + "_created",
					labelNames, c.getKey(), v.created / 1000.0));
		}

		return familySamplesList(Type.HISTOGRAM, samples);
	}

	@Override
	public List<MetricFamilySamples> describe() {
		return Collections
				.singletonList(new MetricFamilySamples(fullname, Type.HISTOGRAM, help,
						Collections.<MetricFamilySamples.Sample> emptyList()));
	}

	double[] getBuckets() {
		return buckets;
	}
}