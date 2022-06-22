package examle.model;

import java.lang.Number;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

public class Point {
	private String measurement;
	private Map<String, String> tags;
	private Map<String, Object> fields;
	private TimeUnit precision = TimeUnit.NANOSECONDS;
	private Number time;

	public String getMeasurement() {
		return measurement;
	}

	public void setMeasurement(String value) {
		measurement = value;
	}

	public Map<String, String> getTags() {
		return tags;
	}

	public void setTags(Map<String, String> value) {
		tags = value;
	}

	public Number getTime() {
		return time;
	}

	public void setTime(Number value) {
		time = value;
	}

	public TimeUnit getPrecision() {
		return precision;
	}

	public void setPrecision(TimeUnit value) {
		precision = value;
	}

	public Map<String, Object> getFields() {
		return fields;
	}

	/* 	public void setFields(Map<String, Object> value) {
			fields = value;
		}
	*/

	// NOTE: Erasure of method setFields(Map<String,String>) is the same as
	// another method in type Point
	// https://stackoverflow.com/questions/21037263/converting-mapstring-string-to-mapstring-object
	public void setFields(Map<String, String> value) {
		fields = new HashMap<>();
		fields.putAll(value);
	}

}
