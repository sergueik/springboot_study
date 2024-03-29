package example;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class Info extends SimpleCollector<Info.Child>
		implements Counter.Describable {

	Info(Builder b) {
		super(b);
	}

	public static class Builder extends SimpleCollector.Builder<Builder, Info> {
		@Override
		public Info create() {
			if (!unit.isEmpty()) {
				throw new IllegalStateException("Info metrics cannot have a unit.");
			}
			return new Info(this);
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
		return new Child(labelNames);
	}

	public static class Child {

		private Map<String, String> value = Collections.emptyMap();
		private List<String> labelNames;

		private Child(List<String> labelNames) {
			this.labelNames = labelNames;
		}

		public void info(Map<String, String> v) {
			for (String key : v.keySet()) {
				checkMetricLabelName(key);
			}
			for (String label : labelNames) {
				if (v.containsKey(label)) {
					throw new IllegalArgumentException(
							"Info and its value cannot have the same label name.");
				}
			}
			this.value = v;
		}

		public void info(String... v) {
			if (v.length % 2 != 0) {
				throw new IllegalArgumentException(
						"An even number of arguments must be passed");
			}
			Map<String, String> m = new TreeMap<String, String>();
			for (int i = 0; i < v.length; i += 2) {
				m.put(v[i], v[i + 1]);
			}
			info(m);
		}

		public Map<String, String> get() {
			return value;
		}
	}

	// Convenience methods.
	public void info(String... v) {
		noLabelsChild.info(v);
	}

	public void info(Map<String, String> v) {
		noLabelsChild.info(v);
	}

	public Map<String, String> get() {
		return noLabelsChild.get();
	}

	@Override
	public List<MetricFamilySamples> collect() {
		List<MetricFamilySamples.Sample> samples = new ArrayList<MetricFamilySamples.Sample>();
		for (Map.Entry<List<String>, Child> c : children.entrySet()) {
			Map<String, String> v = c.getValue().get();
			List<String> names = new ArrayList<String>(labelNames);
			List<String> values = new ArrayList<String>(c.getKey());
			for (Map.Entry<String, String> l : v.entrySet()) {
				names.add(l.getKey());
				values.add(l.getValue());
			}
			samples.add(new MetricFamilySamples.Sample(fullname + "_info", names,
					values, 1.0));
		}

		return familySamplesList(Type.INFO, samples);
	}

	@Override
	public List<MetricFamilySamples> describe() {
		return Collections.singletonList(new MetricFamilySamples(fullname,
				Type.INFO, help, Collections.<MetricFamilySamples.Sample> emptyList()));
	}

}
