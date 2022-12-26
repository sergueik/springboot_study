package example;

import java.io.IOException;
import java.io.Reader;
import java.util.Hashtable;

public class XmlPullParser implements SimplePullParser {

	static final private String UNEXPECTED_EOF = "Unexpected EOF";

	public boolean relaxed;
	private Hashtable entityMap;
	private int depth;
	private String[] elementStack = new String[4];

	private Reader reader;
	private boolean allowEntitiesInAttributes;

	private char[] srcBuf = new char[Runtime.getRuntime().freeMemory() >= 1048576
			? 8192 : 128];

	private int srcPos;
	private int srcCount;

	private boolean eof;

	private int line;
	private int column;

	private int peek0;
	private int peek1;

	// txtbuffer

	private char[] txtBuf = new char[128];
	private int txtPos;

	// Event-related

	private int type;
	private String text;
	private boolean isWhitespace;
	private String name;

	private boolean degenerated;
	private int attributeCount;
	private String[] attributes = new String[16];

	private String[] TYPES = { "Start Document", "End Document", "Start Tag",
			"End Tag", "Text" };

	private final int read() throws IOException {

		int r = this.peek0;
		this.peek0 = this.peek1;

		if (this.peek0 == -1) {
			this.eof = true;
			return r;
		} else if (r == '\n' || r == '\r') {
			this.line++;
			this.column = 0;
			if (r == '\r' && this.peek0 == '\n')
				this.peek0 = 0;
		}
		this.column++;

		if (this.srcPos >= this.srcCount) {
			this.srcCount = this.reader.read(this.srcBuf, 0, this.srcBuf.length);
			if (this.srcCount <= 0) {
				this.peek1 = -1;
				return r;
			}
			this.srcPos = 0;
		}

		this.peek1 = this.srcBuf[this.srcPos++];
		return r;
	}

	private final void exception(String desc) throws IOException {
		throw new IOException(desc + " pos: " + getPositionDescription());
	}

	private final void push(int c) {
		if (c == 0)
			return;

		if (this.txtPos == this.txtBuf.length) {
			char[] bigger = new char[this.txtPos * 4 / 3 + 4];
			System.arraycopy(this.txtBuf, 0, bigger, 0, this.txtPos);
			this.txtBuf = bigger;
		}

		this.txtBuf[this.txtPos++] = (char) c;
	}

	private final void read(char c) throws IOException {
		if (read() != c) {
			if (this.relaxed) {
				if (c <= 32) {
					skip();
					read();
				}
			} else {
				exception("expected: '" + c + "'");
			}
		}
	}

	private final void skip() throws IOException {

		while (!this.eof && this.peek0 <= ' ')
			read();
	}

	private final String pop(int pos) {
		String result = new String(this.txtBuf, pos, this.txtPos - pos);
		this.txtPos = pos;
		return result;
	}

	private final String readName() throws IOException {

		int pos = this.txtPos;
		int c = this.peek0;
		if ((c < 'a' || c > 'z') && (c < 'A' || c > 'Z') && c != '_' && c != ':'
				&& !this.relaxed)
			exception("name expected");

		do {
			push(read());
			c = this.peek0;
		} while ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
				|| (c >= '0' && c <= '9') || c == '_' || c == '-' || c == ':'
				|| c == '.');

		return pop(pos);
	}

	private final void parseLegacy(boolean push) throws IOException {

		String req = "";
		int term;

		read(); // <
		int c = read();

		if (c == '?') {
			term = '?';
		} else if (c == '!') {
			if (this.peek0 == '-') {
				req = "--";
				term = '-';
			} else if (this.peek0 == '[') {
				// TODO hack for <![CDATA[]]
				req = "[CDATA[";
				term = ']';
				this.type = SimplePullParser.TEXT;
				push = true;
			} else {
				req = "DOCTYPE";
				term = -1;
			}
		} else {
			if (c != '[')
				exception("cantreachme: " + c);
			req = "CDATA[";
			term = ']';
		}

		for (int i = 0; i < req.length(); i++)
			read(req.charAt(i));

		if (term == -1)
			parseDoctype();
		else {
			while (true) {
				if (this.eof)
					exception(UNEXPECTED_EOF);

				c = read();
				if (push)
					push(c);

				if ((term == '?' || c == term) && this.peek0 == term
						&& this.peek1 == '>')
					break;
			}
			read();
			read();

			if (push && term != '?')
				pop(this.txtPos - 1);
		}
	}

	/** precondition: &lt! consumed */

	private final void parseDoctype() throws IOException {

		int nesting = 1;

		while (true) {
			int i = read();
			switch (i) {

			case -1:
				exception(UNEXPECTED_EOF);
				break;

			case '<':
				nesting++;
				break;

			case '>':
				if ((--nesting) == 0)
					return;
				break;
			}
		}
	}

	/* precondition: &lt;/ consumed */

	private final void parseEndTag() throws IOException {

		read(); // '<'
		read(); // '/'
		this.name = readName();
		if (this.depth == 0 && !this.relaxed)
			exception("element stack empty");

		if (this.name.equals(this.elementStack[this.depth - 1]))
			this.depth--;
		else if (!this.relaxed)
			exception("expected: " + this.elementStack[this.depth]);
		skip();
		read('>');
	}

	private final int peekType() {
		switch (this.peek0) {
		case -1:
			return SimplePullParser.END_DOCUMENT;
		case '&':
			return SimplePullParser.ENTITY_REF;
		case '<':
			switch (this.peek1) {
			case '/':
				return SimplePullParser.END_TAG;
			case '[':
				return SimplePullParser.CDSECT;
			case '?':
			case '!':
				return SimplePullParser.LEGACY;
			default:
				return SimplePullParser.START_TAG;
			}
		default:
			return SimplePullParser.TEXT;
		}
	}

	private static final String[] ensureCapacity(String[] arr, int required) {
		if (arr.length >= required)
			return arr;
		String[] bigger = new String[required + 16];
		System.arraycopy(arr, 0, bigger, 0, arr.length);
		return bigger;
	}

	/** Sets name and attributes */

	private final void parseStartTag() throws IOException {

		read(); // <
		this.name = readName();
		this.elementStack = ensureCapacity(this.elementStack, this.depth + 1);
		this.elementStack[this.depth++] = this.name;

		while (true) {
			skip();

			int c = this.peek0;

			if (c == '/') {
				this.degenerated = true;
				read();
				skip();
				read('>');
				break;
			}

			if (c == '>') {
				read();
				break;
			}

			if (c == -1)
				exception(UNEXPECTED_EOF);

			String attrName = readName();

			if (attrName.length() == 0)
				exception("attr name expected");

			skip();
			read('=');

			skip();
			int delimiter = read();

			if (delimiter != '\'' && delimiter != '"') {
				if (!this.relaxed)
					exception(
							"<" + this.name + ">: invalid delimiter: " + (char) delimiter);

				delimiter = ' ';
			}

			int i = (this.attributeCount++) << 1;

			this.attributes = ensureCapacity(this.attributes, i + 4);

			this.attributes[i++] = attrName;

			int p = this.txtPos;

			if (this.allowEntitiesInAttributes) {
				pushText(delimiter);
			} else {
				pushTextAttribute(delimiter);
			}

			this.attributes[i] = pop(p);

			if (delimiter != ' ')
				read(); // skip endquote
		}
	}

	/**
	 * result: isWhitespace; if the setName parameter is set, the name of the
	 * entity is stored in "name"
	 */

	public final boolean pushEntity() throws IOException {

		read(); // &

		int pos = this.txtPos;

		while (!this.eof && this.peek0 != ';') {
			push(read());
		}

		String code = pop(pos);

		read();

		if (code.length() > 0 && code.charAt(0) == '#') {
			int c = (code.charAt(1) == 'x' ? Integer.parseInt(code.substring(2), 16)
					: Integer.parseInt(code.substring(1)));
			push(c);
			return c <= ' ';
		}

		String result = (String) this.entityMap.get(code);
		boolean whitespace = true;

		if (result == null) {
			result = "&" + code + ";";
		}

		for (int i = 0; i < result.length(); i++) {
			char c = result.charAt(i);
			if (c > ' ') {
				whitespace = false;
			}
			push(c);
		}

		return whitespace;
	}

	/**
	 * types: '<': parse to any token (for nextToken ()) '"': parse to quote ' ':
	 * parse to whitespace or '>'
	 */

	private final boolean pushText(int delimiter) throws IOException {

		boolean whitespace = true;
		int next = this.peek0;

		while (!this.eof && next != delimiter) { // covers eof, '<', '"'

			if (delimiter == ' ')
				if (next <= ' ' || next == '>')
					break;

			if (next == '&') {
				if (!pushEntity())
					whitespace = false;

			} else {
				if (next > ' ')
					whitespace = false;

				push(read());
			}

			next = this.peek0;
		}

		return whitespace;
	}

	private final boolean pushTextAttribute(int delimiter) throws IOException {
		boolean whitespace = true;
		int next = this.peek0;

		while (!this.eof && next != delimiter) { // covers eof, '<', '"'

			if (delimiter == ' ')
				if (next <= ' ' || next == '>')
					break;

			if (next > ' ')
				whitespace = false;

			push(read());
			next = this.peek0;
		}

		return whitespace;
	}

	// --------------- public part starts here... ---------------

	public XmlPullParser(Reader reader) throws IOException {
		this(reader, true);
	}

	public XmlPullParser(Reader reader, boolean allowEntitiesInAttributes)
			throws IOException {
		this.reader = reader;
		this.allowEntitiesInAttributes = allowEntitiesInAttributes;

		this.peek0 = reader.read();
		this.peek1 = reader.read();

		this.eof = this.peek0 == -1;

		this.entityMap = new Hashtable();
		this.entityMap.put("amp", "&");
		this.entityMap.put("apos", "'");
		this.entityMap.put("gt", ">");
		this.entityMap.put("lt", "<");
		this.entityMap.put("quot", "\"");

		this.line = 1;
		this.column = 1;
	}

	public void defineCharacterEntity(String entity, String value) {
		this.entityMap.put(entity, value);
	}

	public int getDepth() {
		return this.depth;
	}

	public String getPositionDescription() {

		StringBuffer buf = new StringBuffer(
				this.type < this.TYPES.length ? this.TYPES[this.type] : "Other");

		buf.append(" @" + this.line + ":" + this.column + ": ");

		if (this.type == SimplePullParser.START_TAG
				|| this.type == SimplePullParser.END_TAG) {
			buf.append('<');
			if (this.type == SimplePullParser.END_TAG)
				buf.append('/');

			buf.append(this.name);
			buf.append('>');
		} else if (this.isWhitespace)
			buf.append("[whitespace]");
		else
			buf.append(getText());

		return buf.toString();
	}

	public int getLineNumber() {
		return this.line;
	}

	public int getColumnNumber() {
		return this.column;
	}

	public boolean isWhitespace() {
		return this.isWhitespace;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see de.enough.polish.xml.SimplePullParser#getText()
	 */
	public String getText() {

		if (this.text == null)
			this.text = pop(0);

		return this.text;
	}

	public String getName() {
		return this.name;
	}

	public boolean isEmptyElementTag() {
		return this.degenerated;
	}

	public int getAttributeCount() {
		return this.attributeCount;
	}

	public String getAttributeName(int index) {
		if (index >= this.attributeCount) {
			throw new IndexOutOfBoundsException();
		}
		return this.attributes[index << 1];
	}

	public String getAttributeValue(int index) {
		if (index >= this.attributeCount) {
			throw new IndexOutOfBoundsException();
		}
		return this.attributes[(index << 1) + 1];
	}

	public String getAttributeValue(String attrName) {

		for (int i = (this.attributeCount << 1) - 2; i >= 0; i -= 2) {
			if (this.attributes[i].equals(attrName))
				return this.attributes[i + 1];
		}

		return null;
	}

	public int getType() {
		return this.type;
	}

	public int next() {

		try {

			if (this.degenerated) {
				this.type = SimplePullParser.END_TAG;
				this.degenerated = false;
				this.depth--;
				return this.type;
			}

			this.txtPos = 0;
			this.isWhitespace = true;

			do {
				this.attributeCount = 0;

				this.name = null;
				this.text = null;
				this.type = peekType();

				switch (this.type) {

				case SimplePullParser.ENTITY_REF:
					this.isWhitespace &= pushEntity();
					this.type = SimplePullParser.TEXT;
					break;

				case SimplePullParser.START_TAG:
					parseStartTag();
					break;

				case SimplePullParser.END_TAG:
					parseEndTag();
					break;

				case SimplePullParser.END_DOCUMENT:
					break;

				case SimplePullParser.TEXT:
					this.isWhitespace &= pushText('<');
					break;

				case SimplePullParser.CDSECT:
					parseLegacy(true);
					this.isWhitespace = false;
					this.type = SimplePullParser.TEXT;
					break;

				default:
					parseLegacy(false);
				}
			} while (this.type > SimplePullParser.TEXT
					|| this.type == SimplePullParser.TEXT
							&& peekType() >= SimplePullParser.TEXT);

			this.isWhitespace &= this.type == SimplePullParser.TEXT;

		} catch (IOException e) {
			this.type = SimplePullParser.END_DOCUMENT;
		}

		return this.type;
	}

	public void require(int eventType, String eventName) throws IOException {

		if (this.type == SimplePullParser.TEXT && eventType != SimplePullParser.TEXT
				&& isWhitespace()) {
			next();
		}

		if (eventType != this.type
				|| (eventName != null && !eventName.equals(getName()))) {
			exception("expected: " + this.TYPES[eventType] + "/" + eventName);
		}
	}

	public String readText() throws IOException {

		if (this.type != SimplePullParser.TEXT)
			return "";

		String result = getText();
		next();
		return result;
	}

}
