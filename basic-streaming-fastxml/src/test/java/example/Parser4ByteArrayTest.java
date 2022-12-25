package example;

import org.junit.Assert;
import org.junit.Test;

import example.FastXmlFactory;
import example.FastXmlParser;
import example.exception.NumberFormatException;
import example.exception.ParseException;
import example.utils.FileLoaderUtils;

import java.io.IOException;
import java.nio.charset.Charset;

public class Parser4ByteArrayTest {
	@Test
	public void testXmlDeclare() throws IOException, ParseException {
		byte[] bytes = FileLoaderUtils.loadClasspathFile("test2.xml");
		FastXmlParser parser = FastXmlFactory.newInstance(bytes, null);
		if (parser.next() == FastXmlParser.START_DOCUMENT) {
			Charset charset = parser.getEncode();
			Assert.assertTrue(
					charset != null && charset.equals(Charset.forName("utf-8")));
		}
	}

	@Test
	public void testSkipCurrentTag() throws IOException, ParseException {
		byte[] totalBytes = FileLoaderUtils.loadClasspathFile("test2.xml");
		FastXmlParser parser = FastXmlFactory.newInstance(totalBytes, null);
		StringBuilder sb = new StringBuilder();

		int packageCount = 0;
		for (int event = parser.next();; event = parser.next()) {
			if (event == FastXmlParser.END_DOCUMENT) {
				Assert.assertEquals(
						"<bix><package sex=\"male\"><id>222</id><name hasEntityReference=\"false\">weager</name></package><package><id>333</id><name/></package></bix>",
						sb.toString());
				return;
			}
			switch (event) {
			case FastXmlParser.START_TAG:
				if ("package".equals(parser.getString())) {
					packageCount++;
					if (packageCount == 1) {
						parser.skipCurrentTag();
						break;
					}
				}
				sb.append('<').append(parser.getString());
				if (parser.getNextEvent() == FastXmlParser.TEXT
						|| parser.getNextEvent() == FastXmlParser.END_TAG
						|| parser.getNextEvent() == FastXmlParser.START_TAG) {
					sb.append('>');
				}
				break;
			case FastXmlParser.END_TAG:
				sb.append("</").append(parser.getString()).append('>');
				break;
			case FastXmlParser.END_TAG_WITHOUT_TEXT:
				sb.append("/>");
				break;
			case FastXmlParser.ATTRIBUTE_NAME:
				sb.append(' ').append(parser.getString()).append("=");
				break;
			case FastXmlParser.ATTRIBUTE_VALUE:
				sb.append('\"').append(parser.getString()).append('\"');
				int nextEvent = parser.getNextEvent();
				if (nextEvent == FastXmlParser.TEXT
						|| nextEvent == FastXmlParser.END_TAG
						|| nextEvent == FastXmlParser.START_TAG) {
					sb.append('>');
				} else if (nextEvent == FastXmlParser.END_TAG_WITHOUT_TEXT) {
					sb.append("/>");
				}
				break;
			case FastXmlParser.TEXT:
				String text = parser.getString();
				if (text != null) {
					sb.append(text);
				}
				break;
			}
		}
	}

	@Test
	public void testGetNumber()
			throws IOException, ParseException, NumberFormatException {
		byte[] totalBytes = FileLoaderUtils.loadClasspathFile("test2.xml");
		FastXmlParser parser = FastXmlFactory.newInstance(totalBytes, null);
		byte[] str111 = "111".getBytes();
		byte[] str222 = "222".getBytes();
		for (int event = parser
				.next(); event != FastXmlParser.END_DOCUMENT; event = parser.next()) {
			if (event == FastXmlParser.TEXT) {
				if (parser.isMatch(str111)) {
					Assert.assertEquals(111, parser.getInt());
				} else if (parser.isMatch(str222)) {
					Assert.assertEquals(222l, parser.getLong());
				}
			}
		}
	}

	@Test
	public void testGetString()
			throws IOException, ParseException, NumberFormatException {
		byte[] totalBytes = FileLoaderUtils.loadClasspathFile("test2.xml");
		FastXmlParser parser = FastXmlFactory.newInstance(totalBytes, null);
		byte[] name1 = "汤姆克鲁兹".getBytes();
		byte[] name2 = "weager".getBytes();
		byte[] age = "age".getBytes();
		byte[] sex = "sex".getBytes();
		byte[] hasEntityReference = "hasEntityReference".getBytes();
		byte[] TRUE = "true".getBytes();
		byte[] FALSE = "false".getBytes();

		for (int event = parser
				.next(); event != FastXmlParser.END_DOCUMENT; event = parser.next()) {
			if (event == FastXmlParser.TEXT) { // text content
				if (parser.isMatch(name1)) {
					Assert.assertEquals("汤姆克鲁兹", parser.getStringWithDecoding());
				} else if (parser.isMatch(name2)) {
					Assert.assertEquals("weager", parser.getString());
				}
			} else if (event == FastXmlParser.END_TAG_WITHOUT_TEXT) { // tagName
				Assert.assertEquals("name", parser.getString());
			}
			if (parser.getNextEvent() == FastXmlParser.ATTRIBUTE_NAME) { // tagName
				String tagName = parser.getString();
				parser.next();
				if (parser.isMatch(sex)) {
					Assert.assertEquals("package", tagName);
					parser.next();
					Assert.assertEquals("male", parser.getString());
				}

			}
			if (parser.getCurrentEvent() == FastXmlParser.ATTRIBUTE_NAME
					&& parser.isMatch(age)) {
				parser.next();
				Assert.assertEquals(null, parser.getString());
			}
			if (parser.getCurrentEvent() == FastXmlParser.ATTRIBUTE_NAME
					&& parser.isMatch(hasEntityReference)) {
				parser.next(); // move to attribute value
				if (parser.isMatch(TRUE)) {
					parser.next(); // move to text
					Assert.assertEquals("  汤姆克鲁兹-&côté &amp;c&#244;t&#233;  ",
							parser.getStringWithDecoding());
				} else if (parser.isMatch(FALSE)) {
					parser.next();
					Assert.assertEquals("weager", parser.getStringWithDecoding());
				}
			}
		}
	}

}
