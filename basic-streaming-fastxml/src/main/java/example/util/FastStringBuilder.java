package example.util;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/util/FastStringBuilder.java 
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public final class FastStringBuilder {
	private char[] chars;
	private int last = 0;

	public FastStringBuilder(int length) {
		chars = new char[length];
	}

	public void append(byte b) {
		chars[last] = (char) b;
		last++;
	}

	public void append(char c) {
		chars[last] = c;
		last++;
	}

	public int length() {
		return last;
	}

	public String toString() {
		return new String(chars, 0, last);
	}
}
