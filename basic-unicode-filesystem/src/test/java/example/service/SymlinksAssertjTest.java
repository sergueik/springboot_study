package example.service;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.HashSet;
import java.util.Set;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;
import org.junit.jupiter.api.Assumptions;

import static org.assertj.core.api.Assertions.assertThat;

class SymlinksAssertjTest {

	@TempDir
	Path tempDir;

	private Path target = null;
	private Path link = null;
	private static String osName = getOSName();

	@BeforeEach
	public void beforeEach() {
		Assumptions.assumeFalse(osName.equals("windows"));
	}

	@DisplayName("Create and read symbolic link")
	@Test
	void test1() throws IOException {
		target = Files.createFile(tempDir.resolve("file.txt"));
		link = tempDir.resolve("link.txt");
		Files.createSymbolicLink(link, target);
		assertThat(Files.isSymbolicLink(link)).as("should be discoverable").isTrue();
		assertThat(Files.readSymbolicLink(link)).as("should point to the target it was created for").isEqualTo(target);
	}

	@DisplayName("Counting everything")
	@Test
	void test2() throws IOException {

		target = Files.createFile(tempDir.resolve("file.txt"));
		link = tempDir.resolve("link.txt");
		Files.createSymbolicLink(link, target);

		long count = Files.walk(tempDir).filter(Files::isRegularFile).count();

		assertThat(count).as("Naive count both file and symlink").isEqualTo(2L);
	}

	@DisplayName("Dedup counting only regular file")
	@Test
	void test3() throws IOException {

		target = Files.createFile(tempDir.resolve("file.txt"));
		link = tempDir.resolve("link.txt");
		Files.createSymbolicLink(link, target);

		Set<Path> visited = new HashSet<>();

		long count = Files.walk(tempDir).filter(Files::isRegularFile).filter(path -> {
			try {
				System.err.println("Counting " + path.getFileName());
				return visited.add(path.toRealPath());
			} catch (IOException e) {
				return false;
			}
		}).count();

		assertThat(count).as("Dedup count sees only the regular file").isEqualTo(1L);
	}

	@DisplayName("Broken symlink points to missing target")
	@Test
	void test4() throws IOException {

		target = Files.createFile(tempDir.resolve("file.txt"));
		link = tempDir.resolve("link.txt");
		Files.createSymbolicLink(link, target);

		Files.delete(target);

		assertThat(Files.isSymbolicLink(link)).as("still recognized as symbolic link").isTrue();

		assertThat(Files.exists(link, LinkOption.NOFOLLOW_LINKS)).as("link itself still exists").isTrue();

		Path resolved = Files.readSymbolicLink(link);

		assertThat(Files.exists(resolved)).as("target does not exist (broken link)").isFalse();
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