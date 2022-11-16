package example.model.grafana;

// generated via https://json2csharp.com/code-converters/json-to-pojo

// NOTE: "legend" is found in several areas of Grafana exported Dashboard JSON:
// "legend": { "show": false }
// "legend": { "avg": false, "current": false, "max": false, "min": false, "show": true, "total": false, "values": false },

public class Legend {
	public boolean total;
	public boolean values;

	public boolean show;
	public boolean avg;
	public boolean current;
	public boolean max;
	public boolean min;

	public boolean isShow() {
		return show;
	}

	public void setShow(boolean data) {
		show = data;
	}

	public boolean isAvg() {
		return avg;
	}

	public void setAvg(boolean data) {
		avg = data;
	}

	public boolean isCurrent() {
		return current;
	}

	public void setCurrent(boolean data) {
		current = data;
	}

	public boolean isMax() {
		return max;
	}

	public void setMax(boolean data) {
		max = data;
	}

	public boolean isMin() {
		return min;
	}

	public void setMin(boolean data) {
		min = data;
	}

	public boolean isTotal() {
		return total;
	}

	public void setTotal(boolean total) {
		this.total = total;
	}

	public boolean isValues() {
		return values;
	}

	public void setValues(boolean values) {
		this.values = values;
	}

}