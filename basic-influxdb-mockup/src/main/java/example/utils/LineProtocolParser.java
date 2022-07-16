package example.utils;

import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.concurrent.TimeUnit;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import examle.model.Point;
import example.controller.Controller;

/**
 * Common utilities class for Line Protocol influxDB mocking
 * @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */

public class LineProtocolParser {

	private final Logger logger = LoggerFactory
			.getLogger(LineProtocolParser.class);

	private static LineProtocolParser instance = new LineProtocolParser();

	private boolean debug = false;

	public void setDebug(boolean value) {
		debug = value;
	}

	private LineProtocolParser() {
	}

	public static LineProtocolParser getInstance() {
		return instance;
	}

	private final static String lineProtocolGrammar = "^([-a-z0-9_]+)((?:,(?:[-a-z0-9A-Z_=\"]+))*) ((?:[-a-z.0-9_]+=[-a-z.0-9_\"]+)(?:,[-a-z0-9_]+=[-a-z.0-9_\"]+)*) ([0-9]+)$";
	private final static String tagGrammar = ",?([-a-z0-9_]+)=([-a-zA-Z0-9_]+)";
	private final static String fieldGrammar = ",?([-a-z0-9_]+)=([-a-zA-Z.0-9_]+)";

	public String parseLineProtocolLine(String input) {
		return parseLineProtocolLine(input, lineProtocolGrammar);
	}

	public Point extractPointFromLineProtocolLine(String input) {
		return extractPointFromLineProtocolLine(input, lineProtocolGrammar);
	}

	public Point extractPointFromLineProtocolLine(String input, String grammar) {
		if (input == null)
			throw new IllegalArgumentException ("input cannot be empty");
		Point result = new Point();
		logger.info("input: " + input);
		if (null != input) {
			Pattern p = Pattern.compile(grammar);
			Matcher m = p.matcher(input);
			StringBuffer sb = new StringBuffer();
			if (m.find()) {
				logger.info("Found match.");
				String measurement = m.group(1);
				String tag_set = m.groupCount() > 1 ? m.group(2) : null;
				String field_set = m.groupCount() > 2 ? m.group(3) : null;
				String timestamp = m.groupCount() > 3 ? m.group(4) : null;

				Map<String, String> tags = unpackTags(tag_set, tagGrammar);
				Map<String, String> fields = unpackTags(field_set, tagGrammar);
				result = new Point();
				// NOTE:
				// convert field values to Object
				// https://stackoverflow.com/questions/21037263/converting-mapstring-string-to-mapstring-object
				result.setFields(fields);
				result.setTags(tags);
				result.setMeasurement(measurement);
				result.setPrecision(TimeUnit.SECONDS);
				result.setTime(Long.parseLong(timestamp));
			}
		}
		return result;
	}

	public String parseLineProtocolLine(String input, String grammar) {
		logger.info("input: " + input);
		if (null == input) {
			return null;
		}
		Pattern p = Pattern.compile(grammar);
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			logger.info("Found match.");
			String measurement = m.group(1);
			String tag_set = m.groupCount() > 1 ? m.group(2) : null;
			String field_set = m.groupCount() > 2 ? m.group(3) : null;
			String timestamp = m.groupCount() > 3 ? m.group(4) : null;
			sb.append(String.format("measurement=%s\n", measurement));
			sb.append(String.format("tag_set=%s\n", tag_set));
			sb.append(String.format("field_set=%s\n", field_set));
			sb.append(String.format("timestamp=%s\n", timestamp));
		}
		return sb.toString();
	}

	public String resolveFields(String input) {
		return resolveFields(input, fieldGrammar);
	}

	public String resolveFields(String input, String grammar) {
		if (null == input) {
			return null;
		}
		Map<String, String> result = unpackTags(input, grammar);
		StringBuffer sb = new StringBuffer();
		for (Entry<String, String> entry : result.entrySet()) {
			String field_key = entry.getKey();
			String field_value = entry.getValue();
			sb.append(String.format("field_key=%s\n", field_key));
			sb.append(String.format("field_value=%s\n", field_value));
		}
		return sb.toString();
	}

	public String resolveTags(String input) {

		return resolveTags(input, tagGrammar);
	}

	public String resolveTags(String input, String grammar) {
		if (null == input) {
			return null;
		}
		Map<String, String> result = unpackTags(input, grammar);
		StringBuffer sb = new StringBuffer();
		for (Entry<String, String> entry : result.entrySet()) {
			String tag_key = entry.getKey();
			String tag_value = entry.getValue();
			sb.append(String.format("tag_key=%s\n", tag_key));
			sb.append(String.format("tag_value=%s\n", tag_value));
		}
		return sb.toString();
	}

	public Map<String, String> unpackTags(String input) {
		return unpackTags(input, tagGrammar);
	}

	public Map<String, String> unpackTags(String input, String grammar) {
		Map<String, String> result = new HashMap<>();
		if (null == input) {
			return null;
		}
		Pattern p = Pattern.compile(grammar);
		Matcher m = p.matcher(input);
		while (m.find()) {

			String tag_key = m.group(1);
			String tag_value = m.group(2);
			result.put(tag_key, tag_value);
		}
		return result;
	}

}
