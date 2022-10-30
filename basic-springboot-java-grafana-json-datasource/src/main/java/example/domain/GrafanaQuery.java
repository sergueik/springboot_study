package example.domain;

// import lombok.Getter;
// import lombok.NoArgsConstructor;
// import lombok.Setter;
import org.springframework.data.domain.Range;

import java.util.List;

// @NoArgsConstructor
// @Getter
// @Setter
public class GrafanaQuery {
	String dashboardId;
	String panelId;
	String timezone;
	String interval;
	int intervalMs;
	int maxDataPoints;
	List<GrafanaTarget> targets;
	GrafanaRange range;

	public GrafanaQuery() {
	}

	public String getDashboardId() {
		return dashboardId;
	}

	public void setDashboardId(String dashboardId) {
		this.dashboardId = dashboardId;
	}

	public String getPanelId() {
		return panelId;
	}

	public void setPanelId(String panelId) {
		this.panelId = panelId;
	}

	public String getTimezone() {
		return timezone;
	}

	public void setTimezone(String timezone) {
		this.timezone = timezone;
	}

	public String getInterval() {
		return interval;
	}

	public void setInterval(String interval) {
		this.interval = interval;
	}

	public int getIntervalMs() {
		return intervalMs;
	}

	public void setIntervalMs(int intervalMs) {
		this.intervalMs = intervalMs;
	}

	public int getMaxDataPoints() {
		return maxDataPoints;
	}

	public void setMaxDataPoints(int maxDataPoints) {
		this.maxDataPoints = maxDataPoints;
	}

	public List<GrafanaTarget> getTargets() {
		return targets;
	}

	public void setTargets(List<GrafanaTarget> targets) {
		this.targets = targets;
	}

	public GrafanaRange getRange() {
		return range;
	}

	public void setRange(GrafanaRange range) {
		this.range = range;
	}
}
