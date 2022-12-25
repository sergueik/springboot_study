package example.exception;

import java.io.IOException;

import example.FastXmlParser;
import example.FastXmlParser4ByteArray;
import example.FastXmlParser4InputStream;

// origin: https://github.com/fastxml/fastxml/blob/master/src/main/java/com/github/fastxml/exception/ParseException.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)

public class ParseException extends Exception {

	private int row = -1;
	private int column = -1;

	public ParseException(String message) {
		super(message);
	}

	public ParseException(Throwable cause) {
		this(cause.getMessage(), cause);
	}

	public ParseException(String message, FastXmlParser parser) {
		this(message, parser, null);
	}

	public ParseException(String message, Throwable cause) {
		super(message, cause);
	}

	public ParseException(String message, FastXmlParser parser, Throwable cause) {
		super(message, cause);
		setRowAndColumn(parser);
	}

	public void setRowAndColumn(FastXmlParser parser) {
		if (parser == null) {
			return;
		}
		if (parser instanceof FastXmlParser4ByteArray) {
			byte[] docBytes = parser.getDocument();
			int length = docBytes.length;
			row = 1;
			int cursor = parser.getCursor();
			int lastNewLine = 1;
			for (int i = 0; i <= cursor; i++) {
				if (docBytes[i] == '\n') {
					row++;
					lastNewLine = i;
				}
			}
			column = cursor - lastNewLine;
		} else {
			FastXmlParser4InputStream parser4InputStream = (FastXmlParser4InputStream) parser;
			row = parser4InputStream.getRow();
			column = parser4InputStream.getColumn();
		}
	}

	@Override
	public String getMessage() {
		return getMessage(super.getMessage());
	}

	protected String getMessage(String message) {
		StringBuilder sb = new StringBuilder();
		// position
		sb.append("line[").append(row).append("], column[").append(column)
				.append("]: ");
		sb.append(message);
		return sb.toString();
	}

	public static ParseException tagNotClosed(FastXmlParser parser) {
		return new ParseException("tag does not close correctly", parser);
	}

	public static ParseException emptyDocument() {
		return new ParseException("document should not be empty");
	}

	public static ParseException otherError(FastXmlParser parser) {
		return new ParseException("Other error: invalid parser state", parser);
	}

	public static ParseException entityError(String message) {
		return new ParseException(message);
	}

	public static ParseException documentEndUnexpected(FastXmlParser parser) {
		return new ParseException("Document end unexpected", parser);
	}

	public static ParseException formatError(String msg) {
		return new ParseException(msg);
	}

	public static ParseException formatError(String msg, FastXmlParser parser) {
		return new ParseException(msg, parser);
	}

	public static ParseException ioException(IOException e) {
		return new ParseException(e);
	}

	public int getRow() {
		return row;
	}

	public int getColumn() {
		return column;
	}

	public void setRow(int row) {
		this.row = row;
	}

	public void setColumn(int column) {
		this.column = column;
	}
}
