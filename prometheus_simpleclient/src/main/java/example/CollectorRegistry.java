package example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

public class CollectorRegistry {
	public static final CollectorRegistry defaultRegistry = new CollectorRegistry(
			true);

	private final Object namesCollectorsLock = new Object();
	private final Map<Collector, List<String>> collectorsToNames = new HashMap<Collector, List<String>>();
	private final Map<String, Collector> namesToCollectors = new HashMap<String, Collector>();

	private final boolean autoDescribe;

	public CollectorRegistry() {
		this(false);
	}

	public CollectorRegistry(boolean autoDescribe) {
		this.autoDescribe = autoDescribe;
	}

	public void register(Collector m) {
		List<String> names = collectorNames(m);
		assertNoDuplicateNames(m, names);
		synchronized (namesCollectorsLock) {
			for (String name : names) {
				if (namesToCollectors.containsKey(name)) {
					throw new IllegalArgumentException(
							"Failed to register Collector of type "
									+ m.getClass().getSimpleName() + ": " + name
									+ " is already in use by another Collector of type "
									+ namesToCollectors.get(name).getClass().getSimpleName());
				}
			}
			for (String name : names) {
				namesToCollectors.put(name, m);
			}
			collectorsToNames.put(m, names);
		}
	}

	private void assertNoDuplicateNames(Collector m, List<String> names) {
		Set<String> uniqueNames = new HashSet<String>();
		for (String name : names) {
			if (!uniqueNames.add(name)) {
				throw new IllegalArgumentException(
						"Failed to register Collector of type "
								+ m.getClass().getSimpleName()
								+ ": The Collector exposes the same name multiple times: "
								+ name);
			}
		}
	}

	public void unregister(Collector m) {
		synchronized (namesCollectorsLock) {
			List<String> names = collectorsToNames.remove(m);
			for (String name : names) {
				namesToCollectors.remove(name);
			}
		}
	}

	public void clear() {
		synchronized (namesCollectorsLock) {
			collectorsToNames.clear();
			namesToCollectors.clear();
		}
	}

	private Set<Collector> collectors() {
		synchronized (namesCollectorsLock) {
			return new HashSet<Collector>(collectorsToNames.keySet());
		}
	}

	private List<String> collectorNames(Collector m) {
		List<Collector.MetricFamilySamples> mfs;
		if (m instanceof Collector.Describable) {
			mfs = ((Collector.Describable) m).describe();
		} else if (autoDescribe) {
			mfs = m.collect();
		} else {
			mfs = Collections.emptyList();
		}

		List<String> names = new ArrayList<String>();
		for (Collector.MetricFamilySamples family : mfs) {
			names.addAll(Arrays.asList(family.getNames()));
		}
		return names;
	}

	public Enumeration<Collector.MetricFamilySamples> metricFamilySamples() {
		return new MetricFamilySamplesEnumeration();
	}

	public Enumeration<Collector.MetricFamilySamples> filteredMetricFamilySamples(
			Set<String> includedNames) {
		return new MetricFamilySamplesEnumeration(new SampleNameFilter.Builder()
				.nameMustBeEqualTo(includedNames).build());
	}

	public Enumeration<Collector.MetricFamilySamples> filteredMetricFamilySamples(
			Predicate<String> sampleNameFilter) {
		return new MetricFamilySamplesEnumeration(sampleNameFilter);
	}

	class MetricFamilySamplesEnumeration
			implements Enumeration<Collector.MetricFamilySamples> {

		private final Iterator<Collector> collectorIter;
		private Iterator<Collector.MetricFamilySamples> metricFamilySamples;
		private Collector.MetricFamilySamples next;
		private final Predicate<String> sampleNameFilter;

		MetricFamilySamplesEnumeration(Predicate<String> sampleNameFilter) {
			this.sampleNameFilter = sampleNameFilter;
			this.collectorIter = filteredCollectorIterator();
			findNextElement();
		}

		private Iterator<Collector> filteredCollectorIterator() {
			if (sampleNameFilter == null) {
				return collectors().iterator();
			} else {
				HashSet<Collector> collectors = new HashSet<Collector>();
				synchronized (namesCollectorsLock) {
					for (Map.Entry<String, Collector> entry : namesToCollectors
							.entrySet()) {
						// Note that namesToCollectors contains keys for all combinations of
						// suffixes (_total, _info, etc.).
						if (sampleNameFilter.test(entry.getKey())) {
							collectors.add(entry.getValue());
						}
					}
				}
				return collectors.iterator();
			}
		}

		MetricFamilySamplesEnumeration() {
			this(null);
		}

		private void findNextElement() {
			next = null;

			while (metricFamilySamples != null && metricFamilySamples.hasNext()) {
				next = metricFamilySamples.next().filter(sampleNameFilter);
				if (next != null) {
					return;
				}
			}

			while (collectorIter.hasNext()) {
				metricFamilySamples = collectorIter.next().collect(sampleNameFilter)
						.iterator();
				while (metricFamilySamples.hasNext()) {
					next = metricFamilySamples.next().filter(sampleNameFilter);
					if (next != null) {
						return;
					}
				}
			}
		}

		public Collector.MetricFamilySamples nextElement() {
			Collector.MetricFamilySamples current = next;
			if (current == null) {
				throw new NoSuchElementException();
			}
			findNextElement();
			return current;
		}

		public boolean hasMoreElements() {
			return next != null;
		}
	}

	public Double getSampleValue(String name) {
		return getSampleValue(name, new String[] {}, new String[] {});
	}

	public Double getSampleValue(String name, String[] labelNames,
			String[] labelValues) {
		for (Collector.MetricFamilySamples metricFamilySamples : Collections
				.list(metricFamilySamples())) {
			for (Collector.MetricFamilySamples.Sample sample : metricFamilySamples.samples) {
				if (sample.name.equals(name)
						&& Arrays.equals(sample.labelNames.toArray(), labelNames)
						&& Arrays.equals(sample.labelValues.toArray(), labelValues)) {
					return sample.value;
				}
			}
		}
		return null;
	}

}
