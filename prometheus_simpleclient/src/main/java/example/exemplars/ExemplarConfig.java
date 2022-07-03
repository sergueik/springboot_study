package example.exemplars;

public class ExemplarConfig {

	private static volatile boolean enabled = true;
	private static volatile HistogramExemplarSampler histogramExemplarSampler;
	private static volatile CounterExemplarSampler counterExemplarSampler;

	static {
		ExemplarSampler defaultExemplarSampler = new Tracer().initExemplarSampler();
		counterExemplarSampler = defaultExemplarSampler;
		histogramExemplarSampler = defaultExemplarSampler;
	}

	public static void setCounterExemplarSampler(
			CounterExemplarSampler counterExemplarSampler) {
		ExemplarConfig.counterExemplarSampler = counterExemplarSampler;
	}

	public static void setHistogramExemplarSampler(
			HistogramExemplarSampler histogramExemplarSampler) {
		ExemplarConfig.histogramExemplarSampler = histogramExemplarSampler;
	}

	public static void disableExemplars() {
		enabled = false;
	}

	public static void enableExemplars() {
		enabled = true;
	}

	public static CounterExemplarSampler getCounterExemplarSampler() {
		return counterExemplarSampler;
	}

	public static HistogramExemplarSampler getHistogramExemplarSampler() {
		return histogramExemplarSampler;
	}

	public static boolean isExemplarsEnabled() {
		return enabled;
	}
}
