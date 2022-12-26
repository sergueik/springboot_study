package example;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.List;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

import org.testng.ITestResult;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.DataProvider;
import org.testng.annotations.Test;

public class ParserTest {

	private List<String> attributes = new ArrayList<>();

	@DataProvider(name = "wsdl-file-argument-provider", parallel = false)
	public Object[][] wsdl_files() throws Exception {
		return new Object[][] { { "test1.xml" }, { "test2.xml" } };
	}

	@DataProvider(name = "soap-file-argument-provider", parallel = false)
	public Object[][] soap_files() throws Exception {
		return new Object[][] { { "test3.xml" }, { "test4.xml" } };
	}

	@BeforeMethod
	public void beforeTest() throws IOException {
		attributes.clear();
	}

	@Test(enabled = true, dataProvider = "wsdl-file-argument-provider", threadPoolSize = 2)
	public void test1(String fileName) throws IOException {
		String data = getScriptContent(fileName);
		XmlPullParser parser = new XmlPullParser(new StringReader(data));
		do {
			parser.next();
			String description = parser.getPositionDescription();
			System.err.println(description);
			if (parser.getType() == XmlPullParser.TEXT
					|| parser.getType() == XmlPullParser.END_DOCUMENT)
				continue;
			String name = parser.getName();
			if (parser.getType() == XmlPullParser.START_TAG
					&& (name.contains("definitions") || name.contains("operation")
							|| name.contains("service"))) {
				int attributeCount = parser.getAttributeCount();
				assertThat(attributeCount, greaterThan(0));
				System.err
						.println(String.format("Attributes: (%d total)", attributeCount));
				for (int index = 0; index != attributeCount; index++) {
					String attributeName = parser.getAttributeName(index);
					String attributeValue = parser.getAttributeValue(index);
					System.err.println(attributeName + ": " + attributeValue);
					if (attributeName.matches("soapAction")) {
						attributes.add(attributeValue);
					}
				}
			}
		} while (parser.getType() != XmlPullParser.END_DOCUMENT);
		assertThat(attributes.size(), greaterThan(0));
	}

	@Test(enabled = true, dataProvider = "soap-file-argument-provider", threadPoolSize = 2)
	public void test3(String fileName) throws IOException {

		String data = getScriptContent(fileName);
		XmlPullParser parser = new XmlPullParser(new StringReader(data));
		do {
			parser.next();
			String description = parser.getPositionDescription();
			System.err.println(description);
			if (parser.getType() == XmlPullParser.TEXT
					|| parser.getType() == XmlPullParser.END_DOCUMENT)
				continue;
			String name = parser.getName();

			if (parser.getType() == XmlPullParser.START_TAG
					&& name.matches("^[\\w]+:.*") && !name.matches("^soap:")) {
				// TODO: capture the tag name prefix, locate the "xmlns:prefix"
				// attribute
				int attributeCount = parser.getAttributeCount();
				if (attributeCount > 0) {
					System.err
							.println(String.format("Attributes: (%d total)", attributeCount));
					for (int index = 0; index != attributeCount; index++) {
						String attributeName = parser.getAttributeName(index);
						String attributeValue = parser.getAttributeValue(index);
						System.err.println(attributeName + ": " + attributeValue);
					}
				}
			}
		} while (parser.getType() != XmlPullParser.END_DOCUMENT);
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
