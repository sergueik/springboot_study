package example.service;

import java.io.File;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.security.DigestInputStream;
import java.security.MessageDigest;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.stereotype.Service;

@Service
public class DigestService {
	private static final Logger logger = LoggerFactory.getLogger(DigestService.class);

	public String digest(String inputFile) throws Exception {
		String hash = null;
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
		hash = byteArrayToHex(digest);
		logger.info("Digest input: {} hash: {}", inputFile, hash);
		long end = System.currentTimeMillis();
		logger.info("Processed {} bytes in {} ms", new File(Paths.get(inputFile).toString()).length(), (end - start));
		return hash;
	}

	public static String byteArrayToHex(byte[] bytes) {
		StringBuilder sb = new StringBuilder(bytes.length * 2);
		for (byte b : bytes) {
			sb.append(String.format("%02X", b));
		}
		return sb.toString();
	}
}
