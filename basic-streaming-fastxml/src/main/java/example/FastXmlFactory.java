package example;

import java.io.InputStream;
import java.nio.charset.Charset;

import example.exception.ParseException;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/FastXmlFactory.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public class FastXmlFactory {

	public static FastXmlParser newInstance(byte[] docBytes)
			throws ParseException {
		return newInstance(docBytes, null);
	}

	public static FastXmlParser newInstance(byte[] docBytes, Charset charset)
			throws ParseException {
		FastXmlParser4ByteArray parser = new FastXmlParser4ByteArray();
		parser.setInput(docBytes, charset);
		return parser;
	}

	public static FastXmlParser newInstance(InputStream is)
			throws ParseException {
		return newInstance(is, FastXmlParser4InputStream.DEFAULT_BUFFER_SIZE, null);
	}

	public static FastXmlParser newInstance(InputStream is, Charset charset)
			throws ParseException {
		return newInstance(is, FastXmlParser4InputStream.DEFAULT_BUFFER_SIZE,
				charset);
	}

	public static FastXmlParser newInstance(InputStream is, int bufferSize)
			throws ParseException {
		return newInstance(is, bufferSize);
	}

	public static FastXmlParser newInstance(InputStream is, int bufferSize,
			Charset charset) throws ParseException {
		FastXmlParser4InputStream parser = new FastXmlParser4InputStream();
		parser.setInput(is, bufferSize, charset);
		return parser;
	}

}
