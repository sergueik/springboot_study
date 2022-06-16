package example.utils;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.controller.Controller;

/**
 * Common utilities class for Line Protocol influxDB mocking
 * @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */

public class Utils {

	private final Logger logger = LoggerFactory.getLogger(Utils.class);

	private static Utils instance = new Utils();

	private boolean debug = false;

	public void setDebug(boolean value) {
		debug = value;
	}

	private Utils() {
	}

	public static Utils getInstance() {
		return instance;
	}

	private final static String lineProtocolGrammar = "^([-a-z0-9_]+)(?:(?:,([-a-z0-9A-Z_=\"]+))*) (?:([-a-z.0-9_]+=[-a-z.0-9_\"]+)(?:,[-a-z0-9_]+=[-a-z.0-9_\"]+)*) ([0-9]+)$";
	private final static String tagGrammar = "([-a-z0-9_]+)=([-a-zA-Z0-9_]+)(?:,([-a-zA-Z0-9_=\"]+)*)";
	private final static String fieldGrammar = "([-a-z0-9_]+)=([-a-zA-Z.0-9_]+)(?: ([-a-zA-Z0-9_=\"]+)*)";

	public String parseLineProtocolLine(String input) {
		return parseLineProtocolLine(input, lineProtocolGrammar);
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
		Pattern p = Pattern.compile(grammar);
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String field_key = m.group(1);
			String field_value = m.group(2);
			String field_set = m.groupCount() > 2 ? m.group(3) : null;
			sb.append(String.format("field_key=%s\n", field_key));
			sb.append(String.format("field_value=%s\n", field_value));
			sb.append(String.format("field_set=%s\n", field_set));
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
		Pattern p = Pattern.compile(grammar);
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String tag_key = m.group(1);
			String tag_value = m.group(2);
			String tag_set = m.groupCount() > 2 ? m.group(3) : null;
			sb.append(String.format("tag_key=%s\n", tag_key));
			sb.append(String.format("tag_value=%s\n", tag_value));
			sb.append(String.format("tag_set=%s\n", tag_set));
		}
		return sb.toString();
	}

}
