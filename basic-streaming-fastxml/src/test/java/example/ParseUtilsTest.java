package example;

import org.junit.Assert;
import org.junit.Test;

import example.exception.NumberFormatException;
import example.exception.ParseException;
import example.util.ParseUtils;

import java.io.UnsupportedEncodingException;
import java.nio.charset.Charset;
import java.nio.charset.UnsupportedCharsetException;

public class ParseUtilsTest {
	@Test
	public void testParseString() throws UnsupportedEncodingException,
			UnsupportedCharsetException, ParseException {

		// CDATA block is the segment of a string
		String testString = "aaa<![CDATA[bbb]]>ccc";
		byte[] testBytes = testString.getBytes("utf-8");
		String expectString = "aaabbbccc";
		Assert.assertEquals(expectString, ParseUtils.parseStringWithDecoding(
				testBytes, 0, testBytes.length, Charset.forName("utf-8")));
		Assert.assertEquals(expectString,
				ParseUtils.parseString(testBytes, 0, testBytes.length));

		// CDATA contains the text content
		testString = "<![CDATA[这里是CDATA block]]>";
		testBytes = testString.getBytes("utf-8");
		expectString = "这里是CDATA block";
		Assert.assertEquals(expectString, ParseUtils.parseStringWithDecoding(
				testBytes, 0, testBytes.length, Charset.forName("utf-8")));

		// a xml tag in CDATA block
		testString = "<![CDATA[这里是<a href=\"#\">CDATA block</a>]]>";
		testBytes = testString.getBytes("utf-8");
		expectString = "这里是<a href=\"#\">CDATA block</a>";
		Assert.assertEquals(expectString, ParseUtils.parseStringWithDecoding(
				testBytes, 0, testBytes.length, Charset.forName("utf-8")));

		// utf-8 encoding
		testString = "人们都在干嘛aaaa。<![CDATA[我怎么知道呢?]]>你问我我问谁?";
		testBytes = testString.getBytes("utf-8");
		expectString = "人们都在干嘛aaaa。我怎么知道呢?你问我我问谁?";
		Assert.assertEquals(expectString, ParseUtils.parseStringWithDecoding(
				testBytes, 0, testBytes.length, Charset.forName("utf-8")));

		// multiple CDATA block
		testString = "人们都在干嘛aaaa。<![CDATA[我怎么知道呢?<li>test</li>]]>你问我.<![CDATA[我怎么知道呢?]]>我问谁?";
		testBytes = testString.getBytes("utf-8");
		expectString = "人们都在干嘛aaaa。我怎么知道呢?<li>test</li>你问我.我怎么知道呢?我问谁?";
		Assert.assertEquals(expectString, ParseUtils.parseStringWithDecoding(
				testBytes, 0, testBytes.length, Charset.forName("utf-8")));
	}

	@Test
	public void testParseInt() throws NumberFormatException, ParseException {
		byte[] testBytes = "123".getBytes();
		Assert.assertEquals(123,
				ParseUtils.parseInt(testBytes, 0, testBytes.length));

		testBytes = "".getBytes();
		try {
			ParseUtils.parseInt(testBytes, 0, testBytes.length);
			Assert.assertTrue(false);
		} catch (Exception e) {
			Assert.assertTrue(true);
		}

		testBytes = "asdf".getBytes();
		try {
			ParseUtils.parseInt(testBytes, 0, testBytes.length);
			Assert.assertTrue(false);
		} catch (Exception e) {
			Assert.assertTrue(true);
		}

		testBytes = "000123".getBytes();
		try {
			int result = ParseUtils.parseInt(testBytes, 0, testBytes.length);
			Assert.assertEquals(123, result);
		} catch (Exception e) {
			Assert.assertTrue(false);
		}
	}

	@Test
	public void testParseLong() throws NumberFormatException, ParseException {
		byte[] testBytes = "123".getBytes();
		Assert.assertEquals(123l,
				ParseUtils.parseLong(testBytes, 0, testBytes.length));

		testBytes = "".getBytes();
		try {
			ParseUtils.parseLong(testBytes, 0, testBytes.length);
			Assert.assertTrue(false);
		} catch (Exception e) {
			Assert.assertTrue(true);
		}

		testBytes = "asdf".getBytes();
		try {
			ParseUtils.parseLong(testBytes, 0, testBytes.length);
			Assert.assertTrue(false);
		} catch (Exception e) {
			Assert.assertTrue(true);
		}

		testBytes = "000123".getBytes();
		try {
			long result = ParseUtils.parseLong(testBytes, 0, testBytes.length);
			Assert.assertEquals(123l, result);
		} catch (Exception e) {
			Assert.assertTrue(false);
		}
	}

	@Test
	public void testParseDouble() throws NumberFormatException, ParseException {
		byte[] testBytes = "127.22".getBytes();
		Assert.assertEquals(127.22d,
				ParseUtils.parseDouble(testBytes, 0, testBytes.length),
				0.00000000000001d);
	}

	public void testParseFloat() throws NumberFormatException, ParseException {
		byte[] testBytes = "127.22".getBytes();
		Assert.assertEquals(127.22f,
				ParseUtils.parseFloat(testBytes, 0, testBytes.length),
				0.000000000000001f);
	}

	@Test
	public void testEntityReference() throws ParseException {
		byte[] testBytes = "&amp;c&#244;t&#233;".getBytes();
		Charset charset = Charset.forName("utf-8");
		Assert.assertEquals("&côté", ParseUtils.parseStringWithDecoding(testBytes,
				0, testBytes.length, charset));
		Assert.assertEquals("&côté",
				ParseUtils.parseString(testBytes, 0, testBytes.length));

		testBytes = "-&amp;c&#244;t&#233;<![CDATA[ &amp;c&#244;t&#233; ]]>&amp;c&#244;t&#233;"
				.getBytes();
		Assert.assertEquals("-&côté &amp;c&#244;t&#233; &côté", ParseUtils
				.parseStringWithDecoding(testBytes, 0, testBytes.length, charset));
		Assert.assertEquals("-&côté &amp;c&#244;t&#233; &côté",
				ParseUtils.parseString(testBytes, 0, testBytes.length));
	}
}
