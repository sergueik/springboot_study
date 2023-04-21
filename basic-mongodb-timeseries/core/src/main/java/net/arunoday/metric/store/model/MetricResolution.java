package net.arunoday.metric.store.model;

/**
 * Resolution for Metric data
 * 
 * @author Aparna Chaudhary
 */
public enum MetricResolution {

	/** Yearly aggregate */
	YEAR("yearly"),
	/** Monthly aggregate */
	MONTH("monthly"),
	/** Weekly aggregate */
	WEEK("weekly"),
	/** Daily Aggregate */
	DAY("daily"),
	/** Hourly Aggregate */
	HOUR("hourly"),
	/** Aggregation per minute */
	MINUTE("minute");

	private String code;

	/**
	 * Constructor
	 * @param code metric code
	 */
	private MetricResolution(String code) {
		this.code = code;
	}

	/**
	 * Returns resolution code
	 * 
	 * @return metric resolution code
	 */
	public String getCode() {
		return code;
	}
	
	/**
	 * Constructs {@link MetricResolution} based on code.
	 * 
	 * @param code resolution code
	 * @return {@link MetricResolution} for the code; {@literal null} if none found.
	 */
	public static MetricResolution fromCode(String code) {
		if (code != null) {
			for (MetricResolution metric : MetricResolution.values()) {
				if (code.equalsIgnoreCase(metric.code)) {
					return metric;
				}
			}
		}
		return null;
	}

	@Override
	public String toString() {
		return code;
	}

}
