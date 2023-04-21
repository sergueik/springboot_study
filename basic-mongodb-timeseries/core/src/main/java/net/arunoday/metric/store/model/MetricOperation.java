package net.arunoday.metric.store.model;

/**
 * Default supported aggregation operations.
 * 
 * @author Aparna Chaudhary
 */
public enum MetricOperation {

	/** Aggregation operation to calculate minimum value */
	MIN("min"),
	/** Aggregation operation to calculate maximum value */
	MAX("max"),
	/** Aggregation operation to calculate sum */
	SUM("sum"),
	/** Aggregation operation to calculate average value */
	AVG("avg");

	private String operation;

	/**
	 * Private constructor
	 * 
	 * @param operation aggregation operation
	 */
	private MetricOperation(String operation) {
		this.operation = operation;
	}

	/**
	 * Returns aggregation operation
	 * 
	 * @return aggregation operation
	 */
	public String getOperation() {
		return operation;
	}

	/**
	 * Constructs {@link MetricOperation} based on operation.
	 * 
	 * @param operation aggregation operation
	 * @return {@link MetricOperation} for the operation; {@literal null} if none found.
	 */
	public static MetricOperation fromOperation(String operation) {
		if (operation != null) {
			for (MetricOperation metric : MetricOperation.values()) {
				if (operation.equalsIgnoreCase(metric.operation)) {
					return metric;
				}
			}
		}
		return null;
	}

	@Override
	public String toString() {
		return operation;
	}

}
