package example.service;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.util.Random;

import org.junit.jupiter.api.*;
import org.junit.jupiter.api.io.TempDir;

import static org.assertj.core.api.Assertions.*;

class FilesystemServiceAssertjTest {

	private final static Random rand = new Random();

	@TempDir
	Path tmpDir;

	private String filename = null;
	private Path file = null;
	private static String osName = getOSName();

	@BeforeEach
	public void beforeEach() throws Exception {
		// nothing needed, @TempDir handles it
	}

	@AfterEach
	public void afterEach() throws Exception {
		// nothing needed, @TempDir handles cleanup safely
	}

	@DisplayName("valid Unicode filename - Canadian French")
	@Test
	void test1() throws Exception {
		// shouldHandleCanadianFrenchUnicodeInFilename
		filename = "rapport_école_été_2024.txt";

		file = tmpDir.resolve(filename);

		assertThatCode(() -> Files.writeString(file, "ok")).doesNotThrowAnyException();

		assertThat(Files.exists(file)).isTrue();
	}

	@DisplayName("Invalid Unicode - unpaired surrogate")
	@Test
	void test2() throws Exception {
		// shouldRejectInvalidUnicodeUnpairedSurrogateInFilename
		Assumptions.assumeFalse(osName.equals("windows"));

		filename = "bad_\uD800_name.txt";

		assertThatThrownBy(() -> {
			file = tmpDir.resolve(filename);
			Files.writeString(file, "ok");
		}).isInstanceOf(InvalidPathException.class);
	}

	@DisplayName("Encoding mismatch at filesystem boundary")
	@Test
	void test3() throws Exception {
		// shouldExposeEncodingMismatch_onFilesystem
		Assumptions.assumeFalse(osName.equals("windows"));

		// bytes produced as Latin-1
		byte[] latin1 = "é".getBytes(StandardCharsets.ISO_8859_1);

		// wrongly decoded as UTF-8
		String misdecoded = new String(latin1, StandardCharsets.UTF_8);

		file = tmpDir.resolve("file_" + misdecoded + ".txt");

		assertThatCode(() -> Files.writeString(file, "data")).doesNotThrowAnyException();

		// key assertion: corruption occurred
		assertThat(file.getFileName().toString()).doesNotContain("é");
	}

	@DisplayName("Malformed filename via raw bytes (Linux)")
	@Test
	void test4() throws Exception {
		// shouldCreateMalformedFilename_LinuxOnly
		Assumptions.assumeTrue(osName.contains("linux"));

		Path valid = tmpDir.resolve("tempfile");
		Files.writeString(valid, "ok");

		byte[] badBytes = new byte[] { (byte) 0xC3, (byte) 0x28 };

		Path invalidPath = tmpDir.resolve(new String(badBytes, StandardCharsets.ISO_8859_1));

		try {
			Files.createSymbolicLink(invalidPath, valid);
		} catch (Exception e) {
			throw new org.opentest4j.TestAbortedException("Symlinks not supported: " + e.getMessage());
		}

		assertThatCode(() -> {
			try (DirectoryStream<Path> stream = Files.newDirectoryStream(tmpDir)) {
				for (Path p : stream) {
					p.toString(); // force decoding
				}
			}
		}).doesNotThrowAnyException();
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
}