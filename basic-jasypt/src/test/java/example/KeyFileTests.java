package example;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.io.BufferedWriter;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.core.IsNot.not;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

public class KeyFileTests {
	// echo 'hello, world' | base64 -
	private final String data = "aGVsbG8sIHdvcmxkCg==";
	private File file;
	private String text;

	// https://stackoverflow.com/questions/16365155/removing-a-newline-character-at-the-end-of-a-file
	// truncate -s -1 <<file>>
	@Before
	public void before() throws IOException {
		file = File.createTempFile("key", "txt");
	}

	@After
	public void after() {
		file.delete();
	}

	@Test
	public void test() throws IOException {
		saveString2File(file, data + "  \n\r\n\n\n");

		System.err.println("Written " + file.length() + " chars of data");
		String filePath = file.getAbsolutePath();
		Path resource = Paths.get(filePath);
		byte[] payload = Files.readAllBytes(resource);
		text = new String(payload, "UTF-8");
		text = text.split("\r?\n")[0].replaceAll("\\s\\s*", "");
		System.err
				.println("Read " + text.length() + " chars of data after truncation");
		assertThat("failed to truncate", text, is(data));
	}

	// origin:http://www.java2s.com/ref/java/java-file-write-text-to-file.html
	public static final boolean saveString2File(File file, String content) {
		try {
			BufferedWriter bw = new BufferedWriter(
					new OutputStreamWriter(new FileOutputStream(file)));
			bw.write(content);
			bw.close();
			return true;
		} catch (Exception e) {
			return false;
		}
	}
}
