package example.component;

import java.util.List;

public class QueryTimeserieResponse {
	private String target;
	private List<List<Double>> datapoints;

	// Metric value as a float , unixtimestamp in milliseconds
	public String getTarget() {
		return target;
	}

	public void setTarget(String data) {
		target = data;
	}

	public List<List<Double>> getDatapoints() {
		return datapoints;
	}

	public void setDatapoints(List<List<Double>> data) {
		datapoints = data;
	}
}