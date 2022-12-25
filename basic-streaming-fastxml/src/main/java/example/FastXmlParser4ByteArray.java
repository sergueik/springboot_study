package example;

import java.nio.charset.Charset;

import example.exception.NumberFormatException;
import example.exception.ParseException;
import example.util.ByteUtils;
import example.util.ParseUtils;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/FastXmlParser4ByteArray.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public class FastXmlParser4ByteArray extends AbstractFastXmlParser {

	public void setInput(byte[] bytes, Charset charset) throws ParseException {
		if (bytes == null || bytes.length == 0) {
			throw ParseException.emptyDocument();
		}
		// init
		this.docBytes = bytes;
		this.cursor = 0;
		this.currentIndex = 0;
		this.currentBytesLength = 0;
		this.currentEvent = END_DOCUMENT;
		this.nextEvent = START_DOCUMENT;
		this.currentDepth = 0;
		this.charset = charset;
		this.docBytesLength = bytes.length;
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
		if (docBytes[cursor] == '<') {
			if (docBytes[cursor + 1] == '?'
					&& (docBytes[cursor + 2] == 'x' || docBytes[cursor + 2] == 'X')
					&& (docBytes[cursor + 3] == 'm' || docBytes[cursor + 3] == 'M')
					&& (docBytes[cursor + 4] == 'l' || docBytes[cursor + 4] == 'L')) {
				cursor += 5;
				skipUselessChar();

				if (charset != null) {
					return processEndDeclaration();
				} else {
					for (; cursor < docBytesLength; cursor++) {
						if ((docBytes[cursor] == 'e' || docBytes[cursor] == 'E')
								&& (docBytes[cursor + 1] == 'n' || docBytes[cursor + 1] == 'N')
								&& (docBytes[cursor + 2] == 'c' || docBytes[cursor + 2] == 'C')
								&& (docBytes[cursor + 3] == 'o' || docBytes[cursor + 3] == 'O')
								&& (docBytes[cursor + 4] == 'd' || docBytes[cursor + 4] == 'D')
								&& (docBytes[cursor + 5] == 'i' || docBytes[cursor + 5] == 'I')
								&& (docBytes[cursor + 6] == 'n' || docBytes[cursor + 6] == 'N')
								&& (docBytes[cursor + 7] == 'g'
										|| docBytes[cursor + 7] == 'G')) {
							cursor += 8; // skip "encoding"
							skipUselessChar();
							if (docBytes[cursor] == '=') {
								cursor++;
								skipUselessChar();
								byte currentCursor = docBytes[cursor];
								if (currentCursor == '\"' || currentCursor == '\'') {
									processEncodingValue();
									return processEndDeclaration();
								} else {
									throw ParseException.formatError("need '\"' or '\'' here",
											this);
								}
							} else {
								throw ParseException.formatError("need '=' here", this);
							}
						} else if (docBytes[cursor] == '?' && docBytes[cursor + 1] == '>') {
							cursor += 2;
							skipUselessChar();
							return _processEndDeclaration();
						}
					}
					throw ParseException.formatError(
							"xml declaration should contain encoding, or specify charset on method setInput(byte[], Charset)",
							this);
				}
			} else {
				cursor++;
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
		for (; cursor < docBytesLength; cursor++) {
			if (docBytes[cursor] == '?' && docBytes[cursor + 1] == '>') {
				cursor += 2;
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
		if (docBytes[cursor] == '<') {
			cursor++;
			return START_TAG;
		} else {
			throw ParseException.formatError("should be a <tagName here", this);
		}
	}

	private void processEncodingValue() throws ParseException {
		currentInDoubleQuote = docBytes[cursor] == '\"';
		cursor++;
		currentIndex = cursor;
		for (; cursor < docBytesLength; cursor++) {
			byte cursorByte = docBytes[cursor];
			if ((currentInDoubleQuote && cursorByte == '\"')
					|| (!currentInDoubleQuote && cursorByte == '\'')) {
				currentBytesLength = cursor - currentIndex;
				// length of attribute value
				try {
					charset = Charset.forName(this.getString());
				} catch (Exception e) {
					throw ParseException.formatError(
							"encoding is not found or charset is not correct", this);
				}
				cursor++; // skip another '\'' or '\"'
				return;
			}
		}
		throw ParseException.formatError("need another quotation", this);
	}

	private int processStartTag() throws ParseException {
		for (; cursor < docBytesLength; cursor++) {
			if (!ByteUtils.isValidTokenChar(docBytes[cursor])) {
				if (docBytes[cursor] == '>') { // start tag
					currentBytesLength = cursor - currentIndex;
					cursor++;
					return processAfterStartTag();
				} else {
					int skipCharCount = skipUselessChar();
					// tagName should not contain whitespace
					currentBytesLength = cursor - skipCharCount - currentIndex;
					if (docBytes[cursor] == '/') { // tag end immediately
						cursor++;
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
		for (; cursor < docBytesLength; cursor++) {
			if (docBytes[cursor] == '>') {// the tag end
				currentBytesLength = cursor - currentIndex;
				cursor++;
				return processAfterEndTag();
			} else if (!ByteUtils.isValidTokenChar(docBytes[cursor])) {
				throw ParseException
						.formatError("tag name should not contain invalid char", this);
			}
		}
		throw ParseException.documentEndUnexpected(this);
	}

	private int processEndTagWithoutText() throws ParseException {
		if (docBytes[cursor] == '>') {
			cursor++;
			return processAfterEndTag();
		} else {
			throw ParseException.tagNotClosed(this);
		}
	}

	private int processAfterStartTag() throws ParseException {
		int tempCursor = cursor;
		skipUselessChar();
		// continue to find out next event: another start tag or end tag or text
		if (docBytes[cursor] == '<') {
			byte nextByte = docBytes[cursor + 1];
			if (ByteUtils.isValidTokenChar(nextByte)) { // found out another start tag
				cursor++; // skip "<"
				return START_TAG;
			} else if (nextByte == '/') { // found out end tag
				cursor += 2; // skip "</"
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
		if (cursor == docBytesLength) {
			return END_DOCUMENT;
		} else if (docBytes[cursor] == '<') {
			if (docBytes[cursor + 1] == '/') { // found another end tag
				cursor += 2; // skip "</"
				return END_TAG;
			} else { // found a start tag
				cursor++;
				return START_TAG;
			}
		} else {
			throw ParseException.formatError("need a start tag or end document here",
					this);
		}
	}

	private int processAttributeName() throws ParseException {
		cursor++;
		for (; cursor < docBytesLength; cursor++) {
			if (!ByteUtils.isValidTokenChar(docBytes[cursor])) {
				currentBytesLength = cursor - currentIndex;
				skipUselessChar();
				if (docBytes[cursor] == '=') {
					cursor++;
					skipUselessChar();
					if (docBytes[cursor] == '\"' || docBytes[cursor] == '\'') {
						return ATTRIBUTE_VALUE;
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
		cursor++;
		for (; cursor < docBytesLength; cursor++) {
			byte cursorByte = docBytes[cursor];
			if ((currentInDoubleQuote && cursorByte == '\"')
					|| (!currentInDoubleQuote && cursorByte == '\'')) {
				currentBytesLength = cursor - currentIndex;
				cursor++;
				// continue to read byte until find next event
				skipUselessChar();
				cursorByte = docBytes[cursor];
				if (ByteUtils.isValidTokenChar(cursorByte)) {// next attributeName
					return ATTRIBUTE_NAME;
				} else if (cursorByte == '>') { // the start tag
					cursor++;
					return processAfterStartTag();
				} else if (cursorByte == '/') {// found end tag
					cursor++;
					return END_TAG_WITHOUT_TEXT;
				} else {
					throw ParseException.formatError(
							"should be space or '>' or '/>' or another attribute here", this);
				}
			} else if (cursorByte == '&') {
				currentHasEntityReference = true;
			}
		}
		throw ParseException.formatError("need another quotation", this);
	}

	private int processText() throws ParseException {
		boolean inCDATA = false;
		for (; cursor < docBytesLength; cursor++) {
			byte currentCursor = docBytes[cursor];
			if (inCDATA) { // in CDATA block, then find out "]]>"
				if (currentCursor == ']' && docBytes[cursor + 1] == ']'
						&& docBytes[cursor + 2] == '>') {
					cursor += 2;
					inCDATA = false;
				}
			} else { // not in CDATA block
				if (currentCursor == '<') {
					byte nextByte = docBytes[cursor + 1];
					if (nextByte == '!' && docBytes[cursor + 2] == '['
							&& docBytes[cursor + 3] == 'C' && docBytes[cursor + 4] == 'D'
							&& docBytes[cursor + 5] == 'A' && docBytes[cursor + 6] == 'T'
							&& docBytes[cursor + 7] == 'A' && docBytes[cursor + 8] == '[') { // found
																																								// CDATA
																																								// block
						cursor += 8;
						inCDATA = true;
					} else if (nextByte == '/') { // found end tag
						currentBytesLength = cursor - currentIndex;
						cursor += 2; // skip "</"
						return END_TAG;
					}
				} else if (currentCursor == '&') { // text content contains entity
																						// reference
					currentHasEntityReference = true;
				}
			}
		}
		throw ParseException.documentEndUnexpected(this);
	}

	private int skipUselessChar() throws ParseException {
		int beginIndex = cursor;
		for (; cursor < docBytesLength; cursor++) {
			byte cursorByte = docBytes[cursor];
			if (ByteUtils.isWhiteSpaceOrNewLine(cursorByte)) { 
				// continue
			} else if (cursorByte == '<' && docBytes[cursor + 1] == '!') {
				skipOtherUselessChar();
			} else { // found valid char
				break;
			}
		}
		return cursor - beginIndex;
	}

	private void skipOtherUselessChar() throws ParseException {
		if (docBytes[cursor + 2] == '-' && docBytes[cursor + 3] == '-') {
			cursor += 4; // skip "<!--"
			skipComment();
			// continue
		} else if (docBytes[cursor + 2] == 'D' && docBytes[cursor + 3] == 'O'
				&& docBytes[cursor + 4] == 'C' && docBytes[cursor + 5] == 'T'
				&& docBytes[cursor + 6] == 'Y' && docBytes[cursor + 7] == 'P'
				&& docBytes[cursor + 8] == 'E') { // found DTD DOCTYPE
			cursor += 8; // skip "<!DOCTYPE"
			skipDocType();
			// continue
		}
	}

	private void skipDocType() throws ParseException {
		boolean docTypeDefineInDoc = false;
		for (; cursor < docBytesLength; cursor++) {
			if (!docTypeDefineInDoc && docBytes[cursor] == '[') {
				docTypeDefineInDoc = true;
			} else if (docTypeDefineInDoc) {
				boolean foundEndBracket = false;
				for (; cursor < docBytesLength; cursor++) {
					if (!foundEndBracket && docBytes[cursor] == ']') {
						foundEndBracket = true;
					} else if (foundEndBracket && docBytes[cursor] == '>') {
						return;
					}
				}
			} else if (docBytes[cursor] == '>') { // doctype end
				return;
			}
		}
		throw ParseException.formatError("DTD DOCTYPE does not closed", this);
	}

	private void skipComment() throws ParseException {
		for (; cursor < docBytesLength; cursor++) {
			if (docBytes[cursor] == '-' && docBytes[cursor + 1] == '-'
					&& docBytes[cursor + 2] == '>') { // comment end
				cursor += 2; // skip "-->"
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
