package example;

import org.junit.Test;

import example.FastXmlFactory;
import example.FastXmlParser;
import example.exception.ParseException;
import example.utils.FileLoaderUtils;

import java.io.IOException;

// origin: https://github.com/fastxml/fastxml/blob/master/src/test/java/function/TraverseXml4ByteArrayTest.java
// original copyright: fastXml author(https://github.com/fastxml/fastxml)
public class TraverseXml4ByteArrayTest {

	@Test
	public void testTraverseXml() throws IOException, ParseException {
		boolean printInfo = false;
		traverseXml("bioinfo.xml", printInfo);
		traverseXml("book.xml", printInfo);
		traverseXml("form.xml", printInfo);
		traverseXml("nav.xml", printInfo);
		traverseXml("order.xml", printInfo);
		traverseXml("soap.xml", printInfo);
		traverseXml("wsdl.xml", true);
		traverseXml("test1.xml", printInfo);
		traverseXml("test1-gbk.xml", printInfo);
		traverseXml("test2.xml", printInfo);
		traverseXml("test2-no-declaration.xml", printInfo);
	}

	private void traverseXml(String fileName, boolean printInfo)
			throws ParseException, IOException {
		byte[] doc = FileLoaderUtils.loadClasspathFile(fileName);
		if (doc == null || doc.length == 0) {
			throw ParseException.emptyDocument();
		}

		System.out.println(
				"============[" + fileName + "] begin traverse test============");

		FastXmlParser parser = FastXmlFactory.newInstance(doc);
		while (parser.next() != FastXmlParser.END_DOCUMENT) {
			if (printInfo) {
				String event;
				switch (parser.getCurrentEvent()) {
				case FastXmlParser.START_DOCUMENT:
					event = "start_document";
					break;
				case FastXmlParser.END_DOCUMENT:
					event = "end_document";
					break;
				case FastXmlParser.START_TAG:
					event = "start_tag";
					break;
				case FastXmlParser.END_TAG:
					event = "end_tag";
					break;
				case FastXmlParser.END_TAG_WITHOUT_TEXT:
					event = "end_tag_without_text";
					break;
				case FastXmlParser.ATTRIBUTE_NAME:
					event = "attribute_name";
					break;
				case FastXmlParser.ATTRIBUTE_VALUE:
					event = "attribute_value";
					break;
				case FastXmlParser.TEXT:
					event = "text";
					break;
				default:
					event = "";
				}
				System.out
						.println("[" + event + "]: " + parser.getStringWithDecoding());
			}
		}

		System.out.println(
				"============[" + fileName + "] end traverse test============\n");
	}
}
