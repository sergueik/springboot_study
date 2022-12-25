package example;

import org.junit.Assert;
import org.junit.Test;

import example.util.ByteUtils;


// origin: https://github.com/fastxml/fastxml/blob/master/src/test/java/function/ByteUtilsTest.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)
public class ByteUtilsTest {
	@Test
	public void testByteType() {
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) 'a'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) 'z'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) 'A'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) 'Z'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) '0'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) '9'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) '.'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) '-'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) '_'));
		Assert.assertEquals(true, ByteUtils.isValidTokenChar((byte) ':'));
		Assert.assertEquals(false, ByteUtils.isValidTokenChar((byte) ('a' - 1)));

		Assert.assertEquals(true, ByteUtils.isWhiteSpaceOrNewLine((byte) ' '));
		Assert.assertEquals(true, ByteUtils.isWhiteSpaceOrNewLine((byte) '\t'));
		Assert.assertEquals(true, ByteUtils.isWhiteSpaceOrNewLine((byte) '\r'));
		Assert.assertEquals(true, ByteUtils.isWhiteSpaceOrNewLine((byte) '\n'));
	}
}
