package example.util;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/util/ByteUtils.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public class ByteUtils {

	private final static byte[] byteType = { 0, // 0
			0, 0, 0, 0, 0, 0, 0, 0, // 1~8
			2, // 9: '\t'
			2, // 10: '\n'
			0, 0, // 11~12
			2, // 13: '\r'
			0, 0, 0, 0, 0, 0, 0, // 14~ 20
			0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 21~30
			0, // 31
			2, // 32: ' '
			0, 0, 0, 0, 0, 0, 0, 0, // 33~40
			0, 0, 0, 0, // 41~44
			1, // 45: '-'
			1, // 46: '.'
			0, // 47
			1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 48~57: '0'~'9'
			1, // 58: ':'
			0, 0, 0, 0, 0, 0, // 59~64
			1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
			1, // 65~90: 'A'~'Z'
			0, 0, 0, 0, // 91~94
			1, // 95: '_'
			0, // 96
			1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
			1, // 97~122: 'a'~'z'
	};
	/*
		SPACE 32
		TAB 9
		RETURN 13
		NEWLINE 10
	 */

	public final static boolean isValidTokenChar(final byte b) {
		return b >= 0 && b <= 122 && (byteType[b] & 1) > 0;
		// return b > 0 && ((b >= 'a' && b <= 'z') || (b >= 'A' && b <= 'Z') || (b
		// >= '0' && b <= '9')
		// || b == ':' || b == '-' || b == '_' || b == '.');
	}

	public final static boolean isWhiteSpaceOrNewLine(final byte b) {
		return b >= 0 && b <= 122 && (byteType[b] & 2) > 0;
	}

}
