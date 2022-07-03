package example;

public class SimpleTimer {
	private final long start;
	static TimeProvider defaultTimeProvider = new TimeProvider();
	private final TimeProvider timeProvider;

	static class TimeProvider {
		long nanoTime() {
			return System.nanoTime();
		}
	}

	// Visible for testing.
	SimpleTimer(TimeProvider timeProvider) {
		this.timeProvider = timeProvider;
		start = timeProvider.nanoTime();
	}

	public SimpleTimer() {
		this(defaultTimeProvider);
	}

	public double elapsedSeconds() {
		return elapsedSecondsFromNanos(start, timeProvider.nanoTime());
	}

	public static double elapsedSecondsFromNanos(long startNanos, long endNanos) {
		return (endNanos - startNanos) / Collector.NANOSECONDS_PER_SECOND;
	}
}
