package example;

/**
 * Copyright 2022 Serguei Kouzmine
 */

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

import java.util.regex.Matcher;
import java.util.regex.Pattern;

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

	@Test(enabled = false, dataProvider = "wsdl-file-argument-provider", threadPoolSize = 2)
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
			System.err.println("name: " + name);
			// igore soap namespace
			if (parser.getType() == XmlPullParser.START_TAG
					&& name.matches("^[\\w]+:.*") && !(name.matches("^soap:.*"))) {
				String namespacePrefix = getNamespacePrefix(name);
				System.err.println("namespacePrefix: " + namespacePrefix);
				// TODO: capture the tag name prefix, locate the "xmlns:prefix"
				// attribute
				int attributeCount = parser.getAttributeCount();
				if (attributeCount > 0) {
					System.err
							.println(String.format("Attributes: (%d total)", attributeCount));
					for (int index = 0; index != attributeCount; index++) {
						String attributeName = parser.getAttributeName(index);
						String namespaceName = getNamespaceName(attributeName);
						System.err.println("namespaceName: " + namespaceName);
						// assume for simplicity just one attribute
						assertThat(namespaceName, is(namespacePrefix));
						String attributeValue = parser.getAttributeValue(index);
						System.err.println(attributeName + ": " + attributeValue);
					}
				}
			}
		} while (parser.getType() != XmlPullParser.END_DOCUMENT);
	}

	@Test(enabled = true, dataProvider = "soap-file-argument-provider", threadPoolSize = 2)
	public void test2(String fileName) throws IOException {
		String data = getScriptContent(fileName).replaceAll("(?:\\n|\\r|\\t)", " ");
		String element = "<([\\w]+):([\\w]+)\\s*(?:xmlns:)([\\w]+)\\s*=\\s*\"([^\"]+)\"\\s*(?:.*)>";
		Pattern p = Pattern.compile(element);
		String namespacePrefix = null;
		String tagName = null;
		String namespaceName = null;
		String namespaceUri = null;
		for (String fragment : data.split("<")) {
			String input = "<" + fragment;
			System.err.println("input: " + input);
			Matcher m = p.matcher(input);

			if (m.find()) {
				namespacePrefix = m.group(1);
				tagName = m.group(2);
				namespaceName = m.group(3);
				namespaceUri = m.group(4);
				if (namespacePrefix.matches("soap")) {
					System.err.println("ignoring soap namepace");
					continue;
				}
				System.err.println("namespacePrefix: " + "\"" + namespacePrefix + "\""
						+ "\t" + "tagName: " + "\"" + tagName + "\"" + "\t"
						+ "namespaceName: " + "\"" + namespaceName + "\"" + "\t"
						+ "namespaceUri: " + "\"" + namespaceUri + "\"");
				assertThat(namespaceName, is(namespacePrefix));
			} else {

				System.err.println("no match.");

			}
		}
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

	public static String getNamespaceName(String input) {
		if (null == input) {
			return null;
		}
		String value = null;
		Pattern p = Pattern.compile(String.format("^xmlns:(\\w+)$"));
		Matcher m = p.matcher(input);

		if (m.find()) {
			value = m.group(1);
		}
		return value;
	}

	public static String getNamespacePrefix(String input) {
		if (null == input) {
			return null;
		}
		String value = null;
		Pattern p = Pattern.compile("^(\\w+):");
		Matcher m = p.matcher(input);

		if (m.find()) {
			value = m.group(1);
		}
		return value;
	}

}

