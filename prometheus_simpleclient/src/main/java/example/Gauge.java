package example;

import java.io.Closeable;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;

public class Gauge extends SimpleCollector<Gauge.Child>
		implements Collector.Describable {

	Gauge(Builder b) {
		super(b);
	}

	public static class Builder extends SimpleCollector.Builder<Builder, Gauge> {
		@Override
		public Gauge create() {
			return new Gauge(this);
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
		return new Child();
	}

	public static class Timer implements Closeable {
		private final Child child;
		private final long start;

		private Timer(Child child) {
			this.child = child;
			start = Child.timeProvider.nanoTime();
		}

		public double setDuration() {
			double elapsed = (Child.timeProvider.nanoTime() - start)
					/ NANOSECONDS_PER_SECOND;
			child.set(elapsed);
			return elapsed;
		}
		@Override
		public void close() {
			setDuration();
		}
	}

	public static class Child {

		private final DoubleAdder value = new DoubleAdder();

		static TimeProvider timeProvider = new TimeProvider();

		public void inc() {
			inc(1);
		}

		public void inc(double amt) {
			value.add(amt);
		}

		public void dec() {
			dec(1);
		}

		public void dec(double amt) {
			value.add(-amt);
		}

		public void set(double val) {
			value.set(val);
		}

		public void setToCurrentTime() {
			set(timeProvider.currentTimeMillis() / MILLISECONDS_PER_SECOND);
		}

		public Timer startTimer() {
			return new Timer(this);
		}

		public double setToTime(Runnable timeable) {
			Timer timer = startTimer();

			double elapsed;
			try {
				timeable.run();
			} finally {
				elapsed = timer.setDuration();
			}

			return elapsed;
		}

		public <E> E setToTime(Callable<E> timeable) {
			Timer timer = startTimer();

			try {
				return timeable.call();
			} catch (Exception e) {
				throw new RuntimeException(e);
			} finally {
				timer.setDuration();
			}
		}

		public double get() {
			return value.sum();
		}
	}

	// Convenience methods.
	/**
	 * Increment the gauge with no labels by 1.
	 */
	public void inc() {
		inc(1);
	}

	/**
	 * Increment the gauge with no labels by the given amount.
	 */
	public void inc(double amt) {
		noLabelsChild.inc(amt);
	}

	public void dec() {
		dec(1);
	}

	public void dec(double amt) {
		noLabelsChild.dec(amt);
	}

	public void set(double val) {
		noLabelsChild.set(val);
	}

	public void setToCurrentTime() {
		noLabelsChild.setToCurrentTime();
	}

	public Timer startTimer() {
		return noLabelsChild.startTimer();
	}

	public double setToTime(Runnable timeable) {
		return noLabelsChild.setToTime(timeable);
	}

	public <E> E setToTime(Callable<E> timeable) {
		return noLabelsChild.setToTime(timeable);
	}

	public double get() {
		return noLabelsChild.get();
	}

	@Override
	public List<MetricFamilySamples> collect() {
		List<MetricFamilySamples.Sample> samples = new ArrayList<MetricFamilySamples.Sample>(
				children.size());
		for (Map.Entry<List<String>, Child> c : children.entrySet()) {
			samples.add(new MetricFamilySamples.Sample(fullname, labelNames,
					c.getKey(), c.getValue().get()));
		}
		return familySamplesList(Type.GAUGE, samples);
	}

	@Override
	public List<MetricFamilySamples> describe() {
		return Collections.<MetricFamilySamples> singletonList(
				new GaugeMetricFamily(fullname, help, labelNames));
	}

	static class TimeProvider {
		long currentTimeMillis() {
			return System.currentTimeMillis();
		}

		long nanoTime() {
			return System.nanoTime();
		}
	}
}
