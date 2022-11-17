package example.model.grafonnet;

import java.util.ArrayList;
import java.util.List;

public class HeatmapPanel {
	public String title;
	public String datasource = null;
	public String description = null;
	public String cards_cardPadding = null;
	public String cards_cardRound = null;
	public String color_cardColor = "#b4ff00";
	public String color_colorScale = "sqrt";
	public String color_colorScheme = "interpolateOranges";
	public float color_exponent = (float) 0.5;
	public String color_max = null;
	public String color_min = null;
	public String color_mode = "spectrum";
	public String dataFormat = "timeseries";
	public boolean highlightCards = true;
	public boolean hideZeroBuckets = false;
	public boolean legend_show = false;
	public String minSpan = null;
	public String span = null;
	public String repeat = null;
	public String repeatDirection = null;
	public String tooltipDecimals = null;
	public boolean tooltip_show = true;
	public boolean tooltip_showHistogram = false;
	public boolean xAxis_show = true;
	public String xBucketNumber = null;
	public String xBucketSize = null;
	public String yAxis_decimals = null;
	public String yAxis_format = "short";
	public int yAxis_logBase = 1;
	public String yAxis_min = null;
	public String yAxis_max = null;
	public boolean yAxis_show = true;
	public String yAxis_splitFactor = null;
	public String yBucketBound = "auto";
	public String yBucketNumber = null;
	public String yBucketSize = null;
	public String maxDataPoints = null;
}
