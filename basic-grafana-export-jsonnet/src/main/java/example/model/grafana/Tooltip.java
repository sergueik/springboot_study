package example.model.grafana;

// NOTE: "tooltip" is found in several areas of Grafana exported Dashboard JSON:
// "tooltip": { "show": true, "showHistogram": false },
// "tooltip": { "shared": true,"sort": 0, "value_type": "individual"  },
// this type has members found in both

public class Tooltip {
	public boolean show;
	public boolean showHistogram;
	public boolean shared;
	public int sort;
	public String value_type;

	public boolean isShow() {
		return show;
	}

	public void setShow(boolean data) {
		show = data;
	}

	public boolean isShowHistogram() {
		return showHistogram;
	}

	public void setShowHistogram(boolean data) {
		showHistogram = data;
	}

	public boolean isShared() {
		return shared;
	}

	public void setShared(boolean data) {
		shared = data;
	}

	public int getSort() {
		return sort;
	}

	public void setSort(int data) {
		sort = data;
	}

	public String getValue_type() {
		return value_type;
	}

	public void setValue_type(String data) {
		value_type = data;
	}

}
