package example.utils;

import java.time.Instant;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class Utils {

	public static long dateToEpoch(String strDate) {
		String pattern = "MMM dd yyyy HH:mm:ss.SSS zzz";
		String precision = "millisecond";
		return dateToEpoch(strDate, pattern, precision);
	}
	
	public static long dateToEpoch(String strDate, String pattern) {
		String precision = "millisecond";
		return dateToEpoch(strDate, pattern, precision);
	}
	// java 8
	// based on:
	// https://stackoverflow.com/questions/6687433/convert-a-date-format-in-epoch
	// see also
	// https://docs.oracle.com/javase/8/docs/api/java/time/format/DateTimeFormatter.html

	public static long dateToEpoch(String strDate, String pattern,
			String precision) {
		// String
		DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern(pattern);
		ZonedDateTime zonedDateTime = ZonedDateTime.parse(strDate,
				dateTimeFormatter);
		Instant instant = zonedDateTime.toInstant();
		return precision.equals("second") ? instant.getEpochSecond()
				: instant.toEpochMilli();
	}
	
	public static String resolveEnvVars(String input) {
		if (null == input) {
			return null;
		}
		Pattern p = Pattern.compile("\\$(?:\\{(\\w+)\\}|(\\w+))");
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String envVarName = null == m.group(1) ? m.group(2) : m.group(1);
			String envVarValue = System.getenv(envVarName);
			m.appendReplacement(sb, null == envVarValue ? "" : envVarValue.replace("\\", "\\\\"));
		}
		m.appendTail(sb);
		return sb.toString();
	}
}