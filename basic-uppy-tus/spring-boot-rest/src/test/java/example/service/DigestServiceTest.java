package example.service;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.junit.jupiter.api.Test;


import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.nio.file.Files;
import java.nio.file.Path;

public class DigestServiceTest {
	DigestService digestService = null;

	@Test
	void digestKnownFile() throws Exception {
		digestService = new DigestService();
		Path file = Files.createTempFile("test", ".bin");

		Files.writeString(file, "abc");

		String hash = digestService.digest(file.toString());

		assertThat(hash, is("BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"));
	}
}
