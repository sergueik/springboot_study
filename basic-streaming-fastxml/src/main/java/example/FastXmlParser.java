package example;

import java.nio.charset.Charset;

import example.exception.NumberFormatException;
import example.exception.ParseException;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/FastXmlParser.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public interface FastXmlParser {

	// The following section is event type in fast xml
	int END_DOCUMENT = -1;
	int START_DOCUMENT = 0;
	int START_TAG = 1;
	int END_TAG = 2;
	int END_TAG_WITHOUT_TEXT = 3;
	int ATTRIBUTE_NAME = 4;
	int ATTRIBUTE_VALUE = 5;
	int TEXT = 6;

	byte[] getDocument();

	// current offset of document bytes
	int getCursor();

	// current event that has already checked
	int getCurrentEvent();

	// read bytes, move the cursor, and check it's event type
	int next() throws ParseException;

	int getNextEvent();

	void skipCurrentTag() throws ParseException;

	Charset getEncode();

	int getDepth();

	boolean isMatch(byte[] expectBytes);

	byte[] getRawBytes();

	short getShort() throws NumberFormatException;

	int getInt() throws NumberFormatException;

	float getFloat() throws NumberFormatException;

	double getDouble() throws NumberFormatException;

	long getLong() throws NumberFormatException;

	String getString() throws ParseException;

	String getStringWithDecoding() throws ParseException;

}
