package example.model.grafana;

import java.util.ArrayList;

public class Panel {
	public Cards cards;
	public Color color;
	public String dataFormat;
	public String datasource;
	public FieldConfig fieldConfig;
	public GridPos gridPos;
	public Heatmap heatmap;
	public boolean hideZeroBuckets;
	public boolean highlightCards;
	public int id;
	public Legend legend;
	public String pluginVersion;
	public boolean reverseYBuckets;
	public ArrayList<Target> targets;
	public Object timeFrom;

	public ArrayList<Target> getTargets() {
		return targets;
	}

	public void setTargets(ArrayList<Target> data) {
		targets = data;
	}

	public Object timeShift;
	public String title;
	public Tooltip tooltip;
	public String type;

	public String getType() {
		return type;
	}

	public void setType(String data) {
		type = data;
	}

	public Xaxis xAxis;
	public Object xBucketNumber;
	public Object xBucketSize;
	public Yaxis yAxis;
	public String yBucketBound;
	public Object yBucketNumber;
	public Object yBucketSize;
	public Options options;
	public AliasColors aliasColors;
	public boolean bars;
	public int dashLength;
	public boolean dashes;
	public int fill;
	public int fillGradient;
	public boolean hiddenSeries;
	public boolean lines;
	public int linewidth;
	public String nullPointMode;
	public boolean percentage;
	public int pointradius;
	public boolean points;
	public String renderer;
	public ArrayList<Object> seriesOverrides;
	public int spaceLength;
	public boolean stack;
	public boolean steppedLine;
	public ArrayList<Object> thresholds;
	public ArrayList<Object> timeRegions;
	public Xaxis xaxis;
	public ArrayList<Yaxis> yaxes;
	public Yaxis yaxis;
}
