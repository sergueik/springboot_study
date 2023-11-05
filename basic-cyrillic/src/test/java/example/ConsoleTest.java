package example;

import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

// import org.junit.jupiter.api.Disabled;
// import org.junit.jupiter.api.Test;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;
public class ConsoleTest {

	private static final String message = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345";

	// @Disabled("The forked VM terminated without properly saying goodbye")
	@Test
	public void test() {
		String consoleEncoding = System.getProperty("consoleEncoding");
		if (consoleEncoding != null) {
			try {
				System.setOut(new PrintStream(System.out, true, consoleEncoding));
			System.err.println("Set system output encoding: " + consoleEncoding);
			} catch (UnsupportedEncodingException ex) {
				System.err.println(
						"Unsupported encoding set for console: " + consoleEncoding);
			}
		} else {
			System.err.println("No system setting: " + "consoleEncoding");

		}
		System.out.println(message);
	}

}
