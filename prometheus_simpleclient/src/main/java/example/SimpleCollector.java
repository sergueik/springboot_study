package example;

import java.util.ArrayList;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.Arrays;
import java.util.List;

public abstract class SimpleCollector<Child> extends Collector {
	protected final String fullname;
	protected final String help;
	protected final String unit;
	protected final List<String> labelNames;

	protected final ConcurrentMap<List<String>, Child> children = new ConcurrentHashMap<List<String>, Child>();
	protected Child noLabelsChild;

	/**
	 * Return the Child with the given labels, creating it if needed.
	 * <p>
	 * Must be passed the same number of labels are were passed to {@link #labelNames}.
	 */
	public Child labels(String... labelValues) {
		if (labelValues.length != labelNames.size()) {
			throw new IllegalArgumentException("Incorrect number of labels.");
		}
		for (String label : labelValues) {
			if (label == null) {
				throw new IllegalArgumentException("Label cannot be null.");
			}
		}
		List<String> key = Arrays.asList(labelValues);
		Child c = children.get(key);
		if (c != null) {
			return c;
		}
		Child c2 = newChild();
		Child tmp = children.putIfAbsent(key, c2);
		return tmp == null ? c2 : tmp;
	}

	/**
	 * Remove the Child with the given labels.
	 * <p>
	 * Any references to the Child are invalidated.
	 */
	public void remove(String... labelValues) {
		children.remove(Arrays.asList(labelValues));
		initializeNoLabelsChild();
	}

	/**
	 * Remove all children.
	 * <p>
	 * Any references to any children are invalidated.
	 */
	public void clear() {
		children.clear();
		initializeNoLabelsChild();
	}

	/**
	 * Initialize the child with no labels.
	 */
	protected void initializeNoLabelsChild() {
		// Initialize metric if it has no labels.
		if (labelNames.size() == 0) {
			noLabelsChild = labels();
		}
	}

	public <T extends Collector> T setChild(Child child, String... labelValues) {
		if (labelValues.length != labelNames.size()) {
			throw new IllegalArgumentException("Incorrect number of labels.");
		}
		children.put(Arrays.asList(labelValues), child);
		return (T) this;
	}

	/**
	 * Return a new child, workaround for Java generics limitations.
	 */
	protected abstract Child newChild();

	protected List<MetricFamilySamples> familySamplesList(Collector.Type type,
			List<MetricFamilySamples.Sample> samples) {
		MetricFamilySamples mfs = new MetricFamilySamples(fullname, unit, type,
				help, samples);
		List<MetricFamilySamples> mfsList = new ArrayList<MetricFamilySamples>(1);
		mfsList.add(mfs);
		return mfsList;
	}

	protected SimpleCollector(Builder b) {
		if (b.name.isEmpty())
			throw new IllegalStateException("Name hasn't been set.");
		String name = b.name;
		if (!b.subsystem.isEmpty()) {
			name = b.subsystem + '_' + name;
		}
		if (!b.namespace.isEmpty()) {
			name = b.namespace + '_' + name;
		}
		unit = b.unit;
		if (!unit.isEmpty() && !name.endsWith("_" + unit)) {
			name += "_" + unit;
		}
		fullname = name;
		checkMetricName(fullname);
		if (b.help != null && b.help.isEmpty())
			throw new IllegalStateException("Help hasn't been set.");
		help = b.help;
		labelNames = Arrays.asList(b.labelNames);

		for (String n : labelNames) {
			checkMetricLabelName(n);
		}

		if (!b.dontInitializeNoLabelsChild) {
			initializeNoLabelsChild();
		}
	}

	/**
	 * Builders let you configure and then create collectors.
	 */
	public abstract static class Builder<B extends Builder<B, C>, C extends SimpleCollector> {
		String namespace = "";
		String subsystem = "";
		String name = "";
		String fullname = "";
		String unit = "";
		String help = "";
		String[] labelNames = new String[] {};
		// Some metrics require additional setup before the initialization can be
		// done.
		boolean dontInitializeNoLabelsChild;

		public B name(String name) {
			this.name = name;
			return (B) this;
		}

		public B subsystem(String subsystem) {
			this.subsystem = subsystem;
			return (B) this;
		}

		public B namespace(String namespace) {
			this.namespace = namespace;
			return (B) this;
		}

		public B unit(String unit) {
			this.unit = unit;
			return (B) this;
		}

		public B help(String help) {
			this.help = help;
			return (B) this;
		}

		public B labelNames(String... labelNames) {
			this.labelNames = labelNames;
			return (B) this;
		}

		public abstract C create();

		public C register() {
			return register(CollectorRegistry.defaultRegistry);
		}

		/**
		 * Create and register the Collector with the given registry.
		 */
		public C register(CollectorRegistry registry) {
			C sc = create();
			registry.register(sc);
			return sc;
		}
	}
}
