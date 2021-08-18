package example.component;

import java.util.List;

public class QueryRequest {
	private int panelId;
	private Range range;
	private RangeRaw rangeRaw;
	private String interval;
	private int intervalMs;
	private List<TargetObj> targets;
	private List<AdhocFilter> adhocFilters;
	private String format;
	private int maxDataPoints;

	public int getPanelId() {
		return panelId;
	}

	public void setPanelId(int data) {
		panelId = data;
	}

	public Range getRange() {
		return range;
	}

	public void setRange(Range data) {
		range = data;
	}

	public RangeRaw getRangeRaw() {
		return rangeRaw;
	}

	public void setRangeRaw(RangeRaw data) {
		rangeRaw = rangeRaw;
	}

	public String getInterval() {
		return interval;
	}

	public void setInterval(String data) {
		interval = data;
	}

	public int getIntervalMs() {
		return intervalMs;
	}

	public void setIntervalMs(int data) {
		intervalMs = data;
	}

	public List<TargetObj> getTargets() {
		return targets;
	}

	public void setTargets(List<TargetObj> data) {
		targets = data;
	}

	public List<AdhocFilter> getAdhocFilters() {
		return adhocFilters;
	}

	public void setAdhocFilters(List<AdhocFilter> data) {
		adhocFilters = data;
	}

	public String getFormat() {
		return format;
	}

	public void setFormat(String data) {
		format = data;
	}

	public int getMaxDataPoints() {
		return maxDataPoints;
	}

	public void setMaxDataPoints(int data) {
		maxDataPoints = data;
	}
}