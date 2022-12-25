package example.exception;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/exception/NumberFormatException.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public class NumberFormatException extends ParseException {
	private String rawString;

	public NumberFormatException(String message, Throwable throwable) {
		super(message, throwable);
		this.rawString = rawString;
	}

	public String getRawString() {
		return rawString;
	}

	public void setRawString(String rawString) {
		this.rawString = rawString;
	}

	@Override
	public String getMessage() {
		return getMessage(" [rawString:" + rawString + "]" + super.getMessage());
	}

	public static NumberFormatException formatException(String message,
			Throwable throwable) {
		return new NumberFormatException(message, throwable);
	}
}
