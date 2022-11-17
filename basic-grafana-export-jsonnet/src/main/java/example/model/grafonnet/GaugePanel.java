package example.model.grafonnet;

public class GaugePanel {
	public String title;
	public String description;
	public boolean transparent = false;
	public String datasource; // NOTE\: possibly strongly typed
	public boolean allValues = false;
	public String valueLimit;

	public String fields = "";
	public boolean showThresholdLabels = false;
	public boolean showThresholdMarkers = true;

	public String unit = "percent";
	public int min = 0;
	public int max = 100;
	public int decimals;
	public String displayName;
	public String noValue;
	public String thresholdsMode = "absolute";
	public String repeat;
	public String repeatDirection = "h";
	public int repeatMaxPerRow;
	public String pluginVersion = "7";

}
