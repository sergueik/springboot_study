package net.arunoday.metric.store.model;

/**
 * DTO to represent aggregation results.
 * 
 * @author Aparna Chaudhary
 */
public class AggregationResult {

	private double min;
	private double max;
	private double sum;
	private double avg;

	public AggregationResult() {
		super();
	}

	public AggregationResult(double min, double max, double sum, double avg) {
		this.min = min;
		this.max = max;
		this.sum = sum;
		this.avg = avg;
	}

	public double getMin() {
		return min;
	}

	public void setMin(double min) {
		this.min = min;
	}

	public double getMax() {
		return max;
	}

	public void setMax(double max) {
		this.max = max;
	}

	public double getSum() {
		return sum;
	}

	public void setSum(double sum) {
		this.sum = sum;
	}

	public double getAvg() {
		return avg;
	}

	public void setAvg(double avg) {
		this.avg = avg;
	}

}
