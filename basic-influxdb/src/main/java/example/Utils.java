package example;

import java.time.Instant;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

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
}