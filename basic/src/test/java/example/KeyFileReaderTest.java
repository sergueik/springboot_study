package example;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.Assume;

import example.service.ExampleService;

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

import java.util.Arrays;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.io.UnsupportedEncodingException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

/*
Running example.KeyFileReaderTest
test1: "aGVsbG8sIHdvcmxkCg==
"
21
[B@47089e5f
test2: "aGVsbG8sIHdvcmxkCg==
"
22
[B@4f47d241
test3: "aGVsbG8sIHdvcmxkCg=="
20
[B@4c3e4790
test3: "aGVsbG8sIHdvcmxkCg=="
20
[B@38cccef
test3: "aGVsbG8sIHdvcmxkCg=="
20
[B@5679c6c6
 */
public class KeyFileReaderTest {

	private String payloadHash;
	private String payloadString;
	private byte[] payload;

	// based on:
	// http://www.java2s.com/Tutorials/Java/java.nio.file/Files/Java_Files_readAllBytes_Path_path_.htm
	// see also:

	// echo 'hello, world' | base64 > /c/temp/key/key1.txt;
	// sed -i 's|\r||g'/c/temp/key/key1.txt
	@Test
	public void test1() throws Exception {
		Path wiki_path = Paths.get("C:/temp/key", "key1.txt");

		try {
			payload = Files.readAllBytes(wiki_path);

			payloadString = new String(payload, "UTF-8");
			System.err.println(String.format("test1: \"%s\"\n%d", payloadString,
					payloadString.length()));
			assertThat(payloadString, is("aGVsbG8sIHdvcmxkCg==" + "\n"));
			// simply use toString() -
			// String payloadHash = new String(computeHash(payloadString, "md5"),
			// "ISO-8859-1");
			payloadHash = computeHash(payloadString, "md5").toString();
			System.err.println(payloadHash);
			assertThat(payloadHash, is("[B@47089e5f"));
			// [B@43556938
		} catch (IOException e) {
			System.err.println("Exception(ignored): " + e);
		}

	}

	@Test
	public void test3() throws Exception {
		Path wiki_path = Paths.get("C:/temp/key", "key3.txt");

		try {
			payload = Files.readAllBytes(wiki_path);

			payloadString = new String(payload, "UTF-8");
			System.err.println(String.format("test13: \"%s\"\n%d", payloadString,
					payloadString.length()));
			assertThat(payloadString, is("aGVsbG8sIHdvcmxkCg==" + "\n\r\n"));
			// simply use toString() -
			// String payloadHash = new String(computeHash(payloadString, "md5"),
			// "ISO-8859-1");
			payloadHash = computeHash(payloadString, "md5").toString();
			System.err.println(payloadHash);
			assertThat(payloadHash, is("[B@4c3e4790"));
			// [B@43556938
		} catch (IOException e) {
			System.err.println("Exception(ignored): " + e);
		}

	}

	// echo 'hello, world' | base64 > /c/temp/key/key2.txt;
	// sed -i 's|$|\r|g' /c/temp/key/key2.txt
	@Test
	public void test2() throws Exception {
		Path wiki_path = Paths.get("C:/temp/key", "key2.txt");

		try {
			payload = Files.readAllBytes(wiki_path);

			payloadString = new String(payload, "UTF-8");

			System.err.println(String.format("test2: \"%s\"\n%d", payloadString,
					payloadString.length()));

			assertThat(payloadString, is("aGVsbG8sIHdvcmxkCg==" + "\r\n"));
			// simply use toString() -
			// String payloadHash = new String(computeHash(payloadString, "md5"),
			// "ISO-8859-1");
			payloadHash = computeHash(payloadString, "md5").toString();
			// run again to see if the result is the same
			payloadHash = computeHash(payloadString, "md5").toString();
			System.err.println(payloadHash);
			// "[B@4f47d241"
			assertThat(payloadHash, is("[B@4f47d241"));
		} catch (IOException e) {
			System.err.println("Exception(ignored): " + e.toString());
		}

	}

	// echo 'hello, world' | base64 > /c/temp/key/key2.txt;
	// sed -i 's|$|\r|g' /c/temp/key/key2.txt
	@Test
	public void test4() throws Exception {
		for (String name : Arrays.asList("key2.txt", "key1.txt", "key3.txt")) {
			Path wiki_path = Paths.get("C:/temp/key", name);

			try {
				payload = Files.readAllBytes(wiki_path);

				payloadString = new String(payload, "UTF-8").split("\\r?\\n")[0];
				System.err.println(String.format("test4: \"%s\"\n%d", payloadString,
						payloadString.length()));
				// assertThat(payloadString.lenght(), is(1000));
				assertThat(payloadString, is("aGVsbG8sIHdvcmxkCg=="));
				// simply use toString() -
				// String payloadHash = new String(computeHash(payloadString, "md5"),
				// "ISO-8859-1");
				payloadHash = computeHash(payloadString, "md5").toString();
				System.err.println(payloadHash);
				// "[B@4c3e4790"
				// assertThat(payloadHash, is("[B@38cccef"));
			} catch (IOException e) {
				System.err.println("Exception(ignored): " + e.toString());
			}
		}
	}

	// based on:
	// http://www.java2s.com/example/java-utility-method/md5-hash/md5hashint-string-text-c9ffd.html
	private static byte[] computeHash(String data, String algorithm)
			throws NoSuchAlgorithmException, UnsupportedEncodingException {
		MessageDigest md = MessageDigest.getInstance(algorithm);
		md.update(data.getBytes("UTF-8"), 0, data.length());
		return md.digest();
	}

	public String readFile(String filePath) {
		BufferedReader br = null;
		try {

			br = new BufferedReader(new FileReader(filePath));
			String line = "";
			StringBuilder stringBuilder = new StringBuilder();
			String lineSeparator = System.getProperty("line.separator");
			while ((line = br.readLine()) != null) {
				stringBuilder.append(line);
				stringBuilder.append(lineSeparator);
				// e.g. can have comments etc.
			}
			String fileContents = stringBuilder.toString();
			// positive lookahead keeping the section title and data together
			String[] lines = fileContents.split("\\r?\\n");
			return lines[0];
		} catch (FileNotFoundException e) {
			System.err.println("Exception(ignored): " + e.toString());
			return "";
		} catch (IOException e) {
			System.err.println("Exception(ignored): " + e.toString());
			return "";
		} finally {
			try {
				br.close();
			} catch (IOException e) {
				System.err.println("Exception(ignored): " + e.toString());
			}
		}
	}
}
