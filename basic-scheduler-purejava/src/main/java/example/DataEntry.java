package example;

import java.text.SimpleDateFormat;
import java.util.Date;

public class DataEntry {
	private static final String format = "MM/dd/yyyy HH:mm:ss.SSS";
	private static SimpleDateFormat formatter = new SimpleDateFormat(format);
	private long value;
	private Date date;

	public DataEntry(long value) {
		this.date = new Date();
		this.value = value;
	}

	public long getValue() {
		return value;
	}

	public Date getDate() {
		return date;
	}

	public String getTimestampString() {
		return formatter.format(this.date);
	}

}
