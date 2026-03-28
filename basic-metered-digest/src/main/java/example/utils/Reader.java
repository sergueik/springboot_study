package example.utils;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.io.File;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.DigestInputStream;
import java.security.MessageDigest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Reader {

	private final static Logger logger = LoggerFactory.getLogger(Reader.class);
	private static boolean debug = true;
	String inputFile = "example.bin";

	public Reader(String inputFile) {
		this.inputFile = inputFile;
	}

	public void digest() throws Exception {

		logger.info("Digest input: {}", inputFile);

		long start = System.currentTimeMillis();
		MessageDigest md = MessageDigest.getInstance("SHA-256");
		try (InputStream is = Files.newInputStream(Paths.get(inputFile));
				DigestInputStream dis = new DigestInputStream(is, md)) {

			// Read the stream fully to EOF to update the digest
			byte[] buffer = new byte[8192];
			while (dis.read(buffer) != -1)
				;
		}
		// Final hash
		byte[] digest = md.digest();
		System.err.println(byteArrayToHex(digest));
		long end = System.currentTimeMillis();
		logger.info("Processed {} bytes in {} ms", new File(Paths.get(inputFile).toString()).length(), (end - start));
	}

	public static String byteArrayToHex(byte[] bytes) {
		StringBuilder sb = new StringBuilder(bytes.length * 2);
		for (byte b : bytes) {
			sb.append(String.format("%02X", b));
		}
		return sb.toString();
	}
}