package example;

import org.junit.Assert;
import org.junit.Test;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;

public class ParserTest {
	@Test
	public void test() throws IOException {
		String fileName = "test.xml";
		String data = getScriptContent(fileName);
		XmlPullParser parser = new XmlPullParser(new StringReader(data));

		do {
			parser.next();
			String description = parser.getPositionDescription();
			if (description.contains("whitespace"))
				continue;
			System.err.println(parser.getName());
			if (description.contains("<wsdl:definitions>")
					|| description.contains("<soap:operation>")) {
				int attributeCount = parser.getAttributeCount();
				System.err
						.println(String.format("Attributes: (%d total)", attributeCount));
				for (int index = 0; index != attributeCount; index++) {
					String attributeName = parser.getAttributeName(index);
					String attributeValue = parser.getAttributeValue(index);
					System.err.println(attributeName + ": " + attributeValue);
				}
			}
		} while (parser.getType() != SimplePullParser.END_DOCUMENT);

	}

	protected static String getScriptContent(String filename) {
		try {
			final InputStream stream = ParserTest.class.getClassLoader()
					.getResourceAsStream(filename);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(filename);
		}
	}

}
