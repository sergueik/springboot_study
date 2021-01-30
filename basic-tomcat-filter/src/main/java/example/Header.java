package example;

// based on:
// https://github.com/ggrandes/headers-servlet-filter/blob/master/src/main/java/org/javastack/servlet/filters/ResponseHeadersFilter.java#L118

public class Header {
	final String name;
	final TAG tag;
	final String value;

	public Header(final String name, final TAG tag, final String value) {
		this.name = name;
		this.tag = tag;
		this.value = value;
	}

	public static enum TAG {
		SET, // *default
		SETIFEMPTY, //
		ADD, //
		ADDIFEXIST; //

		static TAG getTag(final String name) {
			try {
				return TAG.valueOf(name.trim().toUpperCase());
			} catch (Exception e) {
			}
			return TAG.SET;
		}
	}

	public static enum TYPE {
		EARLY, // *default
		LATE; //

		static TYPE getType(final String name) {
			try {
				return TYPE.valueOf(name.trim().toUpperCase());
			} catch (Exception e) {
			}
			return TYPE.EARLY;
		}
	}
}
