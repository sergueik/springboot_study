package example.model.grafana;

import java.util.ArrayList;

public class FieldConfig {
	public Defaults defaults;

	public Defaults getDefaults() {
		return defaults;
	}

	public void setDefaults(Defaults data) {
		defaults = data;
	}

	public ArrayList<Object> getOverrides() {
		return overrides;
	}

	public void setOverrides(ArrayList<Object> overrides) {
		this.overrides = overrides;
	}

	public ArrayList<Object> overrides;
}
