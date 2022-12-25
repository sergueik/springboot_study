package example;

import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.Charset;

import example.exception.NumberFormatException;
import example.exception.ParseException;
import example.util.ByteUtils;
import example.util.ParseUtils;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/FastXmlParser4InputStream.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public class FastXmlParser4InputStream extends AbstractFastXmlParser {

	public final static int DEFAULT_BUFFER_SIZE = 8192;
	private InputStream is;
	private int bufferEnd;
	private int lastReadableIndex;
	private int row;
	private int column;
	private int indexOfEOF = -1;

	public void setInput(InputStream is) throws ParseException {
		this.setInput(is, DEFAULT_BUFFER_SIZE, null);
	}

	public void setInput(InputStream is, Charset charset) throws ParseException {
		this.setInput(is, DEFAULT_BUFFER_SIZE, null);
	}

	public void setInput(InputStream is, int bufferSize, Charset charset)
			throws ParseException {
		try {
			if (is == null || is.available() == 0) {
				throw ParseException.emptyDocument();
			}
		} catch (IOException e) {
			throw ParseException.ioException(e);
		}
		if (bufferSize < 1024) { // if the buffer is so small
			bufferSize = DEFAULT_BUFFER_SIZE;
		}
		this.is = is;
		this.charset = charset;
		this.docBytes = new byte[bufferSize];
		this.bufferEnd = bufferSize - 1; // for reuse
		this.cursor = 0;
		this.lastReadableIndex = -1;
		this.indexOfEOF = -1;
		read(); // prefetch a byte for parser
	}

	public int next() throws ParseException {
		try {
			currentEvent = nextEvent;
			currentInDoubleQuote = false;
			currentHasEntityReference = false;
			if (currentEvent != END_TAG_WITHOUT_TEXT) {
				resetCurrent();
			}
			switch (currentEvent) {
			case START_DOCUMENT:
				nextEvent = processStartDocument();
				break;
			case END_DOCUMENT:
				nextEvent = -1;
				break;
			case START_TAG:
				currentDepth++;
				nextEvent = processStartTag();
				break;
			case END_TAG:
				currentDepth--;
				nextEvent = processEndTag();
				break;
			case END_TAG_WITHOUT_TEXT:
				currentDepth--;
				nextEvent = processEndTagWithoutText();
				break;
			case ATTRIBUTE_NAME:
				nextEvent = processAttributeName();
				break;
			case ATTRIBUTE_VALUE:
				nextEvent = processAttributeValue();
				break;
			case TEXT:
				nextEvent = processText();
				break;
			default:
				throw ParseException.otherError(this);
			}
			return currentEvent;
		} catch (ArrayIndexOutOfBoundsException e) {
			throw ParseException.documentEndUnexpected(this);
		}
	}

	private int processStartDocument() throws ParseException {
		skipUselessChar();
		if (readAndCheck(cursor, '<')) {
			if (readAndCheck(cursor + 1, '?') && readAndCheck(cursor + 2, 'x', 'X')
					&& readAndCheck(cursor + 3, 'm', 'M')
					&& readAndCheck(cursor + 4, 'l', 'L')) {
				moveCursor(5);
				skipUselessChar();

				if (charset != null) {// if charset has been set, then just finish
															// declaration.
					return processEndDeclaration();
				} else { // charset has not been set, then find out encoding
					for (; notEnd(); moveCursor()) {
						if (readAndCheck(cursor, 'e', 'E')
								&& readAndCheck(cursor + 1, 'n', 'N')
								&& readAndCheck(cursor + 2, 'c', 'C')
								&& readAndCheck(cursor + 3, 'o', 'O')
								&& readAndCheck(cursor + 4, 'd', 'D')
								&& readAndCheck(cursor + 5, 'i', 'I')
								&& readAndCheck(cursor + 6, 'n', 'N')
								&& readAndCheck(cursor + 7, 'g', 'G')) {
							moveCursor(8); // skip "encoding"
							skipUselessChar();
							if (readAndCheck(cursor, '=')) {
								moveCursor(1);
								skipUselessChar();
								byte currentCursor = docBytes[cursor];
								if (currentCursor == '\"' || currentCursor == '\'') {
									processEncodingValue(); // parse encoding="xxx"
									return processEndDeclaration();
								} else {
									throw ParseException.formatError("need '\"' or '\'' here",
											this);
								}
							} else {
								throw ParseException.formatError("need '=' here", this);
							}
						} else if (readAndCheck(cursor, '?')
								&& readAndCheck(cursor + 1, '>')) {
							moveCursor(2);
							skipUselessChar();
							return _processEndDeclaration();
						}
					}
					throw ParseException.formatError(
							"xml declaration should contain encoding, or specify charset on method setInput(byte[], Charset)",
							this);
				}
			} else { // no declaration, no specified charset, so use the default
								// charset, next event should be START_TAG
				moveCursor(1);
				if (charset == null) {
					charset = DEFAULT_CHARSET;
				}
				return START_TAG; // next event: start tag
			}
		} else {
			throw ParseException.formatError("document should begin with '<'", this);
		}
	}

	private int processEndDeclaration() throws ParseException {
		for (; notEnd(); moveCursor()) {
			if (readAndCheck(cursor, '?') && readAndCheck(cursor + 1, '>')) {
				moveCursor(2);
				skipUselessChar();
				return _processEndDeclaration();
			}
		}
		throw ParseException.documentEndUnexpected(this);
	}

	private int _processEndDeclaration() throws ParseException {
		if (charset == null) {
			charset = DEFAULT_CHARSET;
		}
		if (readAndCheck(cursor, '<')) {
			moveCursor(1);
			return START_TAG;
		} else {
			throw ParseException.formatError("should be a <tagName here", this);
		}
	}

	private void processEncodingValue() throws ParseException {
		// check doubleQuote or singleQuote
		currentInDoubleQuote = docBytes[cursor] == '\"';
		moveCursor(1);
		currentIndex = cursor;
		for (; notEnd(); moveCursor()) {
			byte cursorByte = docBytes[cursor];
			if ((currentInDoubleQuote && cursorByte == '\"')
					|| (!currentInDoubleQuote && cursorByte == '\'')) {// found another
																															// quotation, it's
																															// the end of
																															// attribute value
				currentBytesLength = cursor - currentIndex; // length of attribute value
				try {
					charset = Charset.forName(this.getString());
				} catch (Exception e) {
					throw ParseException.formatError(
							"encoding is not found or charset is not correct", this);
				}
				moveCursor(1); // skip another '\'' or '\"'
				return;
			}
		}
		throw ParseException.formatError("need another quotation", this);
	}

	private int processStartTag() throws ParseException {
		// the first char has bean validated in previous event, so just skip it.
		// to see: processAfterEndTag() and processStartDocument()
		for (; notEnd(); moveCursor()) {
			if (!ByteUtils.isValidTokenChar(docBytes[cursor])) {
				if (readAndCheck(cursor, '>')) { // start tag
					currentBytesLength = cursor - currentIndex;
					moveCursor(1);
					return processAfterStartTag();
				} else {
					int skipCharCount = skipUselessChar();
					// tagName should not contain whitespace
					currentBytesLength = cursor - skipCharCount - currentIndex;
					if (readAndCheck(cursor, '/')) { // tag end immediately
						moveCursor(1);
						return END_TAG_WITHOUT_TEXT;
					} else if (skipCharCount > 0) { // found attribute name
						return ATTRIBUTE_NAME;
					} else {
						throw ParseException.formatError("should be '/' or attribute here",
								this);
					}
				}
			}
		}
		throw ParseException.documentEndUnexpected(this);
	}

	private int processEndTag() throws ParseException {
		for (; notEnd(); moveCursor()) {
			if (readAndCheck(cursor, '>')) {// the tag end
				currentBytesLength = cursor - currentIndex;
				moveCursor(1);
				return processAfterEndTag();
			} else if (!ByteUtils.isValidTokenChar(docBytes[cursor])) {
				throw ParseException
						.formatError("tag name should not contain invalid char", this);
			}
		}
		throw ParseException.documentEndUnexpected(this);
	}

	private int processEndTagWithoutText() throws ParseException {
		if (readAndCheck(cursor, '>')) {
			moveCursor(1);
			return processAfterEndTag();
		} else {
			throw ParseException.tagNotClosed(this);
		}
	}

	private int processAfterStartTag() throws ParseException {
		int tempCursor = cursor;
		int count = skipUselessChar();
		tempCursor = cursor - count; // resetBuffer() may be called, so recaculate
																	// tempCursor
		// continue to find out next event: another start tag or end tag or text
		if (readAndCheck(cursor, '<')) {
			byte nextByte = (byte) read(cursor + 1);
			if (ByteUtils.isValidTokenChar(nextByte)) { // found out another start tag
				moveCursor(1); // skip "<"
				return START_TAG;
			} else if (nextByte == '/') { // found out end tag
				moveCursor(2); // skip "</"
				return END_TAG;
			} else { // so it should be text CDATA block
				// restore
				cursor = tempCursor;
				return TEXT;
			}
		} else {
			// restore
			cursor = tempCursor;
			return TEXT;
		}
	}

	private int processAfterEndTag() throws ParseException {
		skipUselessChar();
		// continue to find out next event: end tag or another start tag or end
		// document
		if (!notEnd()) {
			return END_DOCUMENT;
		} else if (readAndCheck(cursor, '<')) {
			if (readAndCheck(cursor + 1, '/')) { // found another end tag
				moveCursor(2); // skip "</"
				return END_TAG;
			} else { // found a start tag
				moveCursor(1);
				return START_TAG;
			}
		} else {
			throw ParseException.formatError("need a start tag or end document here",
					this);
		}
	}

	private int processAttributeName() throws ParseException {
		moveCursor(1); // the first char has been checked in previous event, so here
										// just skip it
		for (; notEnd(); moveCursor()) {// read tag bytes
			if (!ByteUtils.isValidTokenChar(docBytes[cursor])) {// this attribute name
																													// end
				currentBytesLength = cursor - currentIndex;
				skipUselessChar(); // skip ' ' and '\t' between attribute name and '='
				// read "=\"", '\'' should be ok
				if (readAndCheck(cursor, '=')) {
					moveCursor(1);
					skipUselessChar(); // skip ' ' and '\t' between '=' and attribute
															// value
					if (readAndCheck(cursor, '\"', '\'')) { // found the quotation at the
																									// beginning of attribute
																									// value
						return ATTRIBUTE_VALUE; // found attribute value
					} else {
						throw ParseException.formatError("need '\"' or '\'' here", this);
					}
				} else {
					throw ParseException.formatError("need '=' here", this);
				}
			}
		}
		throw ParseException.documentEndUnexpected(this);
	}

	private int processAttributeValue() throws ParseException {
		// check doubleQuote or singleQuote
		currentInDoubleQuote = docBytes[cursor] == '\"';
		currentIndex++;
		moveCursor(1);
		for (; notEnd(); moveCursor()) {
			byte cursorByte = docBytes[cursor];
			if ((currentInDoubleQuote && cursorByte == '\"')
					|| (!currentInDoubleQuote && cursorByte == '\'')) {
				currentBytesLength = cursor - currentIndex; // length of attribute value
				moveCursor(1);
				// continue to read byte until find next event
				skipUselessChar();
				cursorByte = docBytes[cursor];
				if (ByteUtils.isValidTokenChar(cursorByte)) {// next attributeName
					return ATTRIBUTE_NAME;
				} else if (cursorByte == '>') { // the start tag
					moveCursor(1);
					return processAfterStartTag();
				} else if (cursorByte == '/') {// found end tag
					moveCursor(1);
					return END_TAG_WITHOUT_TEXT;
				} else {
					throw ParseException.formatError(
							"should be space or '>' or '/>' or another attribute here", this);
				}
			} else if (cursorByte == '&') { // attribute value contains entity
																			// reference
				currentHasEntityReference = true;
			}
		}
		throw ParseException.formatError("need another quotation", this);
	}

	private int processText() throws ParseException {
		boolean inCDATA = false;
		for (; notEnd(); moveCursor()) {
			byte currentCursor = docBytes[cursor];
			if (inCDATA) { // in CDATA block, then find out "]]>"
				if (currentCursor == ']' && readAndCheck(cursor + 1, ']')
						&& readAndCheck(cursor + 2, '>')) {
					moveCursor(2);
					inCDATA = false;
				}
			} else { // not in CDATA block
				if (currentCursor == '<') {
					byte nextByte = (byte) read(cursor + 1);
					if (nextByte == '!' && readAndCheck(cursor + 2, '[')
							&& readAndCheck(cursor + 3, 'C') && readAndCheck(cursor + 4, 'D')
							&& readAndCheck(cursor + 5, 'A') && readAndCheck(cursor + 6, 'T')
							&& readAndCheck(cursor + 7, 'A')
							&& readAndCheck(cursor + 8, '[')) { // found CDATA block
						moveCursor(8);
						inCDATA = true;
					} else if (nextByte == '/') { // found end tag
						currentBytesLength = cursor - currentIndex;
						moveCursor(2); // skip "</"
						return END_TAG;
					}
				} else if (currentCursor == '&') {
					currentHasEntityReference = true;
				}
			}
		}
		throw ParseException.documentEndUnexpected(this);
	}

	private int skipUselessChar() throws ParseException {
		int beginIndex = cursor;
		for (; notEnd(); moveCursor()) {
			byte cursorByte = docBytes[cursor];
			if (ByteUtils.isWhiteSpaceOrNewLine(cursorByte)) {
				// continue
			} else if (cursorByte == '<' && readAndCheck(cursor + 1, '!')) {
				skipOtherUselessChar();
			} else { // found valid char
				break;
			}
		}
		return cursor - beginIndex;
	}

	private void skipOtherUselessChar() throws ParseException {
		if (readAndCheck(cursor + 2, '-') && readAndCheck(cursor + 3, '-')) { // found
																																					// comment
			moveCursor(4); // skip "<!--"
			skipComment();
			// continue
		} else if (readAndCheck(cursor + 2, 'D') && readAndCheck(cursor + 3, 'O')
				&& readAndCheck(cursor + 4, 'C') && readAndCheck(cursor + 5, 'T')
				&& readAndCheck(cursor + 6, 'Y') && readAndCheck(cursor + 7, 'P')
				&& readAndCheck(cursor + 8, 'E')) { // found DTD DOCTYPE
			moveCursor(8); // skip "<!DOCTYPE"
			skipDocType();
			// continue
		}
	}

	private void skipDocType() throws ParseException {
		boolean docTypeDefineInDoc = false;
		for (; notEnd(); moveCursor()) {
			if (!docTypeDefineInDoc && readAndCheck(cursor, '[')) {
				docTypeDefineInDoc = true;
			} else if (docTypeDefineInDoc) {
				boolean foundEndBracket = false;
				for (; notEnd(); moveCursor()) {
					if (!foundEndBracket && readAndCheck(cursor, ']')) {
						foundEndBracket = true;
					} else if (foundEndBracket && readAndCheck(cursor, '>')) { // doctype
																																			// end
						return;
					}
				}
			} else if (readAndCheck(cursor, '>')) { // doctype end
				return;
			}
		}
		throw ParseException.formatError("DTD DOCTYPE does not closed", this);
	}

	private void skipComment() throws ParseException {
		for (; notEnd(); moveCursor()) {
			if (readAndCheck(cursor, '-') && readAndCheck(cursor + 1, '-')
					&& readAndCheck(cursor + 2, '>')) { // comment end
				moveCursor(2); // skip "-->"
				return;
			}
		}
		throw ParseException.formatError("comment does not closed", this);
	}

	public void skipCurrentTag() throws ParseException {
		int event; // temp
		int tempDepth = currentDepth - 1; // the depth before this tag
		for (;;) {
			event = next();
			if (currentDepth == tempDepth && event == END_TAG) {
				return;
			}
		}
	}

	private void resetCurrent() {
		currentIndex = cursor;
		currentBytesLength = 0;
	}

	private int moveCursor() throws ParseException {
		int b = moveCursor(1);
		setRowAndColumn(b);
		return b;
	}

	private int moveCursor(int count) throws ParseException {
		cursor += count;
		return read(cursor);
	}

	private boolean notEnd() {
		return cursor != indexOfEOF;
	}

	private boolean readAndCheck(int index, int b1, int b2)
			throws ParseException {
		return readAndCheck(index, b1) || readAndCheck(index, b2);
	}

	private boolean readAndCheck(int index, int b) throws ParseException {
		return b == read(index);
	}

	private int read(int index) throws ParseException {
		// for multiple branch checking, if checking failed in the first branch
		// which has read a byte from IO,
		// then second branch checking has no need to read byte from IO, just check
		// it in buffer
		if (index > lastReadableIndex) {
			return read();
		} else {
			return docBytes[index];
		}
	}

	private int read() throws ParseException {
		if (indexOfEOF > 0) {
			return -1;
		}
		try {
			// buffer is full, so need to adjust buffer to fill any more bytes
			if (lastReadableIndex == this.bufferEnd) {
				if (lastReadableIndex - currentIndex + 1 > currentIndex) {
					// current token is longer than half of the buffer, then need to grow
					// buffer
					growBuffer();
				} else {
					// current token is not longer than half of the buffer, then reset
					// buffer may be better
					resetBuffer();
				}
			}
			int leftLength = bufferEnd - lastReadableIndex;
			int count = is.read(docBytes, lastReadableIndex + 1, leftLength);
			int b = docBytes[lastReadableIndex + 1];
			lastReadableIndex = lastReadableIndex + count;
			if (count < leftLength) {
				indexOfEOF = lastReadableIndex + 1;
			}
			return b;
		} catch (IOException e) {
			throw ParseException.ioException(e);
		}
	}

	private void setRowAndColumn(int bytes) {
		if (bytes == '\n') {
			this.row++;
			this.column = -1;
		}
		this.column++;
	}

	public int getRow() {
		return this.row;
	}

	public int getColumn() {
		return this.column;
	}

	private void resetBuffer() {
		System.arraycopy(docBytes, currentIndex, docBytes, 0,
				lastReadableIndex - currentIndex + 1);
		this.lastReadableIndex = lastReadableIndex - currentIndex;
		if (indexOfEOF > 0) {
			indexOfEOF = this.lastReadableIndex + 1;
		}
		this.cursor = this.cursor - currentIndex;
		this.currentIndex = 0;
		// System.out.println("resetBuffer: " + docBytes.length);
	}

	private void growBuffer() {
		byte[] oldBuffer = this.docBytes;
		int bufferLength = oldBuffer.length;
		this.docBytes = new byte[(int) (oldBuffer.length * 1.75)];
		System.arraycopy(oldBuffer, 0, this.docBytes, 0, bufferLength);
		bufferLength = this.docBytes.length;
		this.bufferEnd = bufferLength - 1;
		// System.out.println("growBuffer: " + docBytes.length);
	}

	public boolean isMatch(byte[] expectBytes) {
		int length = expectBytes.length;
		if (expectBytes.length == currentBytesLength) {
			for (int i = currentIndex, j = 0; j < length; i++, j++) {
				if (docBytes[i] != expectBytes[j]) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	public byte[] getRawBytes() {
		byte[] bytes = new byte[currentBytesLength];
		System.arraycopy(docBytes, currentIndex, bytes, 0, currentBytesLength);
		return bytes;
	}

	public String getString() throws ParseException {
		try {
			return ParseUtils.parseString(docBytes, currentIndex, currentBytesLength);
		} catch (ParseException e) {
			e.setRowAndColumn(this);
			throw e;
		}
	}

	public String getStringWithDecoding() throws ParseException {
		try {
			return ParseUtils.parseStringWithDecoding(docBytes, currentIndex,
					currentBytesLength, charset);
		} catch (ParseException e) {
			e.setRowAndColumn(this);
			throw e;
		}
	}

	public short getShort() throws NumberFormatException {
		try {
			return (short) ParseUtils.parseInt(docBytes, currentIndex,
					currentBytesLength);
		} catch (NumberFormatException e) {
			e.setRowAndColumn(this);
			throw e;
		}
	}

	public int getInt() throws NumberFormatException {
		try {
			return ParseUtils.parseInt(docBytes, currentIndex, currentBytesLength);
		} catch (NumberFormatException e) {
			e.setRowAndColumn(this);
			throw e;
		}
	}

	public long getLong() throws NumberFormatException {
		try {
			return ParseUtils.parseLong(docBytes, currentIndex, currentBytesLength);
		} catch (NumberFormatException e) {
			e.setRowAndColumn(this);
			throw e;
		}
	}

	public float getFloat() throws NumberFormatException {
		try {
			return ParseUtils.parseFloat(docBytes, currentIndex, currentBytesLength);
		} catch (NumberFormatException e) {
			e.setRowAndColumn(this);
			throw e;
		}
	}

	public double getDouble() throws NumberFormatException {
		try {
			return ParseUtils.parseDouble(docBytes, currentIndex, currentBytesLength);
		} catch (NumberFormatException e) {
			e.setRowAndColumn(this);
			throw e;
		}
	}
}
