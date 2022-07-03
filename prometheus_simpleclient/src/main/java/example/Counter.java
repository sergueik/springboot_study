package example;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicReference;

import example.exemplars.CounterExemplarSampler;
import example.exemplars.Exemplar;
import example.exemplars.ExemplarConfig;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;

public class Counter extends SimpleCollector<Counter.Child>
		implements Collector.Describable {

	private final Boolean exemplarsEnabled; // null means default from
																					// ExemplarConfig applies
	private final CounterExemplarSampler exemplarSampler;

	Counter(Builder b) {
		super(b);
		this.exemplarsEnabled = b.exemplarsEnabled;
		this.exemplarSampler = b.exemplarSampler;
		initializeNoLabelsChild();
	}

	public static class Builder
			extends SimpleCollector.Builder<Builder, Counter> {

		private Boolean exemplarsEnabled = null;
		private CounterExemplarSampler exemplarSampler = null;

		@Override
		public Counter create() {
			// Gracefully handle pre-OpenMetrics counters.
			if (name.endsWith("_total")) {
				name = name.substring(0, name.length() - 6);
			}
			dontInitializeNoLabelsChild = true;
			return new Counter(this);
		}

		/**
		 * Enable exemplars and provide a custom {@link CounterExemplarSampler}.
		 */
		public Builder withExemplarSampler(CounterExemplarSampler exemplarSampler) {
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
		return new Child(exemplarsEnabled, exemplarSampler);
	}

	public static class Child {
		private final DoubleAdder value = new DoubleAdder();
		private final long created = System.currentTimeMillis();
		private final Boolean exemplarsEnabled;
		private final CounterExemplarSampler exemplarSampler;
		private final AtomicReference<Exemplar> exemplar = new AtomicReference<Exemplar>();

		public Child() {
			this(null, null);
		}

		public Child(Boolean exemplarsEnabled,
				CounterExemplarSampler exemplarSampler) {
			this.exemplarsEnabled = exemplarsEnabled;
			this.exemplarSampler = exemplarSampler;
		}

		public void inc() {
			inc(1);
		}

		public void incWithExemplar(String... exemplarLabels) {
			incWithExemplar(1, exemplarLabels);
		}

		public void incWithExemplar(Map<String, String> exemplarLabels) {
			incWithExemplar(1, exemplarLabels);
		}

		public void inc(double amt) {
			incWithExemplar(amt, (String[]) null);
		}

		public void incWithExemplar(double amt, String... exemplarLabels) {
			Exemplar exemplar = exemplarLabels == null ? null
					: new Exemplar(amt, System.currentTimeMillis(), exemplarLabels);
			if (amt < 0) {
				throw new IllegalArgumentException(
						"Amount to increment must be non-negative.");
			}
			value.add(amt);
			updateExemplar(amt, exemplar);
		}

		public void incWithExemplar(double amt,
				Map<String, String> exemplarLabels) {
			incWithExemplar(amt, Exemplar.mapToArray(exemplarLabels));
		}

		private void updateExemplar(double amt, Exemplar userProvidedExemplar) {
			Exemplar prev, next;
			do {
				prev = exemplar.get();
				if (userProvidedExemplar == null) {
					next = sampleNextExemplar(amt, prev);
				} else {
					next = userProvidedExemplar;
				}
				if (next == null || next == prev) {
					return;
				}
			} while (!exemplar.compareAndSet(prev, next));
		}

		private Exemplar sampleNextExemplar(double amt, Exemplar prev) {
			if (FALSE.equals(exemplarsEnabled)) {
				return null;
			}
			if (exemplarSampler != null) {
				return exemplarSampler.sample(amt, prev);
			}
			if (TRUE.equals(exemplarsEnabled)
					|| ExemplarConfig.isExemplarsEnabled()) {
				CounterExemplarSampler exemplarSampler = ExemplarConfig
						.getCounterExemplarSampler();
				if (exemplarSampler != null) {
					return exemplarSampler.sample(amt, prev);
				}
			}
			return null;
		}
		public double get() {
			return value.sum();
		}

		private Exemplar getExemplar() {
			return exemplar.get();
		}

		public long created() {
			return created;
		}
	}

	// Convenience methods.

	public void inc() {
		inc(1);
	}

	public void incWithExemplar(String... exemplarLabels) {
		incWithExemplar(1, exemplarLabels);
	}

	public void incWithExemplar(Map<String, String> exemplarLabels) {
		incWithExemplar(1, exemplarLabels);
	}

	public void inc(double amt) {
		noLabelsChild.inc(amt);
	}

	public void incWithExemplar(double amt, String... exemplarLabels) {
		noLabelsChild.incWithExemplar(amt, exemplarLabels);
	}

	public void incWithExemplar(double amt, Map<String, String> exemplarLabels) {
		noLabelsChild.incWithExemplar(amt, exemplarLabels);
	}

	public double get() {
		return noLabelsChild.get();
	}

	@Override
	public List<MetricFamilySamples> collect() {
		List<MetricFamilySamples.Sample> samples = new ArrayList<MetricFamilySamples.Sample>(
				children.size());
		for (Map.Entry<List<String>, Child> c : children.entrySet()) {
			samples
					.add(new MetricFamilySamples.Sample(fullname + "_total", labelNames,
							c.getKey(), c.getValue().get(), c.getValue().getExemplar()));
			samples.add(new MetricFamilySamples.Sample(fullname + "_created",
					labelNames, c.getKey(), c.getValue().created() / 1000.0));
		}
		return familySamplesList(Type.COUNTER, samples);
	}

	@Override
	public List<MetricFamilySamples> describe() {
		return Collections.<MetricFamilySamples> singletonList(
				new CounterMetricFamily(fullname, help, labelNames));
	}
}
