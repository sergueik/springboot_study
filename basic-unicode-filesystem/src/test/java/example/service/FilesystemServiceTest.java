package example.service;

import java.util.Random;

/**
* Copyright 2026 Serguei Kouzmine
*/

import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.springframework.http.ResponseEntity;

import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.DirectoryStream;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.List;
import java.util.Random;
import java.util.UUID;
import java.util.concurrent.atomic.AtomicInteger;

import static org.mockito.Mockito.any;

import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.not;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
//import org.junit.jupiter.api.Ignore;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assumptions;

class FilesystemServiceTest {
	private final static Random rand = new Random();
	private final static Path tmpDir = Path.of(System.getProperty("java.io.tmpdir"))
			.resolve(String.format("tests%d", rand.nextInt(10)));
	// or simply
	@TempDir
	Path tempDir;
	private String filename = null;
	private Path file = null;
	private static String osName = getOSName();

	@BeforeEach
	public void beforeEach() throws Exception {
		// TODO: find out what OS the test run
		// Assume.assumeTrue(!osName.equals("windows"));
		Files.createDirectory(tmpDir);
	}

	@AfterEach
	public void afterEach() throws Exception {
		deleteDirectory(tmpDir);
	}

	@DisplayName("valid Unicode filename - Canadian French")
	@Test
	void test1 /* shouldHandleCanadianFrenchUnicodeInFilename */ () throws Exception {
		filename = "rapport_école_été_2024.txt";
		file = assertDoesNotThrow(() -> {
			return (tmpDir.resolve(filename));
		}, "should not throw exceptions");
		assertDoesNotThrow(() -> Files.writeString(file, "ok"), "should not throw exceptions");
		assertThat(Files.exists(file), is(true));
	}

	@DisplayName("Invalid Unicode - an unpaired surrogate - invalid filename")
	@Test
	void test2 /* shouldRejectInvalidUnicodeUnpairedSurrogateInFilename */ () throws Exception {
		Assumptions.assumeFalse(osName.equals("windows"));
		filename = "bad_\uD800_name.txt"; // invalid UTF-16
		assertThrows(InvalidPathException.class, () -> {
			file = tmpDir.resolve(filename);
			Files.writeString(file, "ok?");
		});
	}

	@DisplayName("Invalid Unicode - an unpaired surrogate - invalid filename")
	@Test
	void test3 /* shouldExposeEncodingMismatch */ () throws Exception {
		String misdecoded = new String("é".getBytes(StandardCharsets.ISO_8859_1), StandardCharsets.UTF_8);
		// typically becomes replacement char
		assertThat(misdecoded, is(not("é")));
	}

	@DisplayName("Invalid Unicode - an unpaired surrogate - invalid filename")
	@Test
	void test4 /* shouldCreateMalformedFilename_LinuxOnly */ () throws Exception {
		Assumptions.assumeTrue(osName.contains("linux"));

		Path valid = tmpDir.resolve("tempfile");
		Files.writeString(valid, "ok");
		// invalid UTF-8 byte sequence in path
		Path invalidPath = tmpDir
				.resolve(new String(new byte[] { (byte) 0xC3, (byte) 0x28 }, StandardCharsets.ISO_8859_1));

		try {
			Files.createSymbolicLink(invalidPath, valid);
		} catch (Exception e) {
			// some FS / containers forbid symlinks → skip
			//
			// Assumptions.abort("Symlinks not supported: " + e.getMessage());
		}

		// now try to read directory
		try (DirectoryStream<Path> stream = Files.newDirectoryStream(tmpDir)) {
			for (Path p : stream) {
				System.out.println("Entry: " + p);
			}
		}
	}

	@DisplayName("Invisible zero width space in filename")
	@Test
	void test5() /* testInvisibleSpaceInFilename */ throws Exception {

		filename = "report.txt";
		Path file = tmpDir.resolve(filename.replaceAll(".", "\u200B" + "."));
		Files.writeString(file, "ok");

		assertThat(file.getFileName().toString(), not(containsString(filename))); // 💣 looks identical
	}

	@DisplayName("Invisible BOM in filename")
	@Test
	void test6 /* testBomInFilename */ () throws Exception {
		String bom = "\uFEFF";
		filename = "report.txt";

		Path file = tmpDir.resolve(bom + filename);
		Files.writeString(file, "ok");

		System.out.println("[" + file.getFileName() + "]");
		assertThat(file.getFileName().toString().indexOf(bom), is(0));
		assertThat(file.getFileName().toString(), matchesPattern(bom + filename));
	}

	@DisplayName("Lookalike characters in filenams - visually identical in many fonts")
	@Test
	void test7()/* testLookAlikeFilenames */ throws Exception {

		String filename1 = "report-file.txt"; // ASCII hyphen '-'
		String filename2 = "report–file.txt"; // EN DASH (U+2013)
												// EM DASH: "report—file.txt" (U+2014)

		Path path1 = tmpDir.resolve(filename1);
		Path path2 = tmpDir.resolve(filename2);

		Files.writeString(path1, "ok");
		Files.writeString(path2, "ok");

		System.out.println("normal: [" + path1.getFileName() + "]");
		System.out.println("sneaky: [" + path2.getFileName() + "]");

		assertThat(path1.getFileName().toString().equals(path2.getFileName().toString()), is(false));

		assertThat(Files.exists(path1), is(true));
		assertThat(Files.exists(path2), is(true));
	}

	@Test
	void test8 /* testMisDecodedFilenameDoubleQuestionMark */() throws Exception {
		// shouldExposeDoubleQuestionMarkEffect

		Path dir = Path.of(System.getProperty("java.io.tmpdir"));

		// bytes that do NOT form valid UTF-8
		byte[] badUtf8 = new byte[] { (byte) 0xC3, (byte) 0x28 };

		// decode incorrectly → replacement chars likely
		String broken = new String(badUtf8, StandardCharsets.UTF_8);

		Path file = dir.resolve("file_" + broken + ".txt");

		Files.writeString(file, "data");

		String actual = file.getFileName().toString();

		System.out.println("Actual filename: " + actual);

		// the important part: it is NOT what we intended
		assertThat(actual, not(containsString("é")));
		assertThat(actual, matchesPattern(".*[\\?�].*")); // '?' or replacement char
	}

	// Utilities
	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

	private void deleteDirectory(Path path) throws IOException {
		if (Files.isDirectory(path)) {
			try (var stream = Files.list(path)) {
				stream.forEach(child -> {
					try {
						deleteDirectory(child);
					} catch (IOException e) {
						throw new RuntimeException(e);
					}
				});
			}
		}
		Files.delete(path);
	}

}