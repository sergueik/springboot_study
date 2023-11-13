package example;

import java.io.PrintStream;
import java.io.UnsupportedEncodingException;
import java.io.PrintStream;
import java.io.UnsupportedEncodingException;

public class ConsoleApp {
	private static final String message = "АБВГДЕЁЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ абвгдеёжзийклмнопрстцуфхцчшщъыьэюя 12345";

	public static void main(String[] args) {
		String consoleEncoding = System.getProperty("consoleEncoding");
		if (consoleEncoding != null) {
			try {
				System.setOut(new PrintStream(System.out, true, consoleEncoding));
			System.err.println("Set system output encoding: " + consoleEncoding);
			} catch (UnsupportedEncodingException ex) {
				System.err.println(
						"Unsupported encoding set for console: " + consoleEncoding);
			}
		}
		System.out.println(message);
	}

}
