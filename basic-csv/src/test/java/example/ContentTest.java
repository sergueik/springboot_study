package example;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.is;

import org.junit.jupiter.api.Test;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;
import org.apache.commons.csv.CSVRecord;

import java.io.FileReader;
import java.io.IOException;
import java.io.Reader;
import java.io.StringWriter;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;

public class ContentTest {

	public static final Map<String, String> AUTHOR_BOOK_MAP = Collections
			.unmodifiableMap(new LinkedHashMap<String, String>() {
				{
					put("Dan Simmons", "Hyperion");
					put("Douglas Adams", "The Hitchhiker's Guide to the Galaxy");
				}
			});
	public static final String[] HEADERS = { "author", "title" };

	enum BookHeaders {
		author, title
	}

	@Test
	void test1() throws IOException {
		Reader in = new FileReader("src/test/resources/book.csv");

		CSVFormat csvFormat = CSVFormat.DEFAULT.builder().setHeader(HEADERS)
				.setSkipHeaderRecord(true).build();
		
		Iterable<CSVRecord> records = csvFormat.parse(in);

		for (CSVRecord record : records) {
			String author = record.get("author");
			String title = record.get("title");
			assertThat(title, is(AUTHOR_BOOK_MAP.get(author)));

		}
	}

}
