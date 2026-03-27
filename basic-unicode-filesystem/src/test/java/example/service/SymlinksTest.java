package example.service;

import java.util.Random;
import java.util.Set;

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
import java.nio.file.LinkOption;
import java.nio.file.Path;
import java.util.Arrays;
import java.util.HashSet;
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

class SymlinksTest {

	@TempDir
	Path tempDir;
	private String filename = null;
	private Path file = null;
	private Path target = null;
	private Path link = null;
	private static String osName = getOSName();

	@BeforeEach
	public void beforeEach() throws Exception {
		Assumptions.assumeFalse(osName.equals("windows"));
	}

	@DisplayName("Create and read symbolic link")
	@Test
	void test1 /* testCreateAndReadSymlink */() throws IOException {
		target = Files.createFile(tempDir.resolve("file.txt"));
		link = tempDir.resolve("link.txt");
		Files.createSymbolicLink(link, target);

		assertThat("should be discoveable", Files.isSymbolicLink(link), is(true));
		assertThat("should be pointing to the target it was created to point to", Files.readSymbolicLink(link),
				is(target));
	}

	@DisplayName("Counting everything")
	@Test
	void test2 /* testsCount */ () throws IOException {

		target = Files.createFile(tempDir.resolve("file.txt"));
		link = tempDir.resolve("link.txt");
		Files.createSymbolicLink(link, target);
		long count = Files.walk(tempDir).filter(Files::isRegularFile).count();
		assertThat("Naive count both file and symlink", count, is(2L));
	}

	@DisplayName("Dedup counting only regular file")
	@Test
	void test3 /* testsCountOnlyRegularFiles */ () throws IOException {
		target = Files.createFile(tempDir.resolve("file.txt"));
		link = tempDir.resolve("link.txt");
		Files.createSymbolicLink(link, target);
		Set<Path> visited = new HashSet<>();
		long count = Files.walk(tempDir).filter(Files::isRegularFile).filter((Path path) -> {
			try {
				System.err.println(String.format("Counting %s", path.getFileName()));
				return visited.add(path.toRealPath());
			} catch (IOException e) {
				// swallow
				return false;
			}
		}).count();
		assertThat("Dedup count sees only the regular file", count, is(1L));
	}

	@DisplayName("Broken symlink points to missing target")
	@Test
	void test4 /* testsCountOnlyRegularFiles */ () throws IOException {

		target = Files.createFile(tempDir.resolve("file.txt"));
		link = tempDir.resolve("link.txt");
		Files.createSymbolicLink(link, target);

		Files.delete(target);

		assertThat("still recognize is symbolic link", Files.isSymbolicLink(link), is(true));
		assertThat("link exists but target does not", Files.exists(link, LinkOption.NOFOLLOW_LINKS), is(true));
		Path resolved = Files.readSymbolicLink(link);
		assertThat("recognize symbolic link is broken", Files.exists(resolved), is(false));
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