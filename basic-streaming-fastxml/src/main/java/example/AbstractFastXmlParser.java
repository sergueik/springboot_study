package example;

import java.nio.charset.Charset;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/AbstractFastXmlParser.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public abstract class AbstractFastXmlParser implements FastXmlParser {
	protected byte[] docBytes;
	protected int cursor;
	protected int currentIndex;
	protected int currentBytesLength;
	protected boolean currentInDoubleQuote;
	protected boolean currentHasEntityReference;
	protected int currentEvent;
	protected int nextEvent;
	protected int currentDepth;
	protected int docBytesLength;
	protected Charset charset;
	protected final static Charset DEFAULT_CHARSET = Charset.defaultCharset();

	public byte[] getDocument() {
		return docBytes;
	}

	public final int getCursor() {
		return cursor;
	}

	public final int getCurrentEvent() {
		return currentEvent;
	}

	public final int getNextEvent() {
		return nextEvent;
	}

	public Charset getEncode() {
		return charset;
	}

	public int getDepth() {
		return currentDepth;
	}
}
