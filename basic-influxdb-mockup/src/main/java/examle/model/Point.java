package examle.model;

import java.util.Map;
import java.util.concurrent.TimeUnit;

public class Point {
	private String measurement;
	private Map<String, String> tags;
	private Number time;
	private TimeUnit precision = TimeUnit.NANOSECONDS;
	private Map<String, Object> fields;
}
