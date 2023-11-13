package example;

import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;

public class ConsoleTest {

	private static final String message = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345";

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
