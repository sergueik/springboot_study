package example;

import org.mockito.Mockito;
import org.mockito.internal.matchers.Any;
import static org.mockito.ArgumentMatchers.anyString;

import example.controller.Worker;

import static org.mockito.Mockito.when;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Properties;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;

import org.junit.Assume;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class MockPropertiesTest {

	Worker worker;
	Properties properties;

	@Before
	public void setup() {
		properties = Mockito.mock(Properties.class);
		when(properties.getProperty(anyString())).thenReturn(null);
	}

	private String filePath = null;

	@Test
	public void test1() {
		filePath = "C:\\tmp\\key.txt";
		Assume.assumeFalse(new File(filePath).exists());
		when(properties.getProperty(anyString(), anyString())).thenReturn("/tmp");
		worker = new Worker(properties);
		assertThat(worker.propertyCheck(),
				is("Data: read error: Invalid path to the file: " + filePath));
	}

	@Test
	// created the directory named "src/test/resources/key.txt" to exercise this
	// test
	public void test2() {

		filePath = System.getProperty("user.dir") + "/src/test/resources/key.txt";
		Assume.assumeTrue(new File(filePath).isDirectory());
		when(properties.getProperty(anyString(), anyString()))
				.thenReturn("src/test/resources");
		worker = new Worker(properties);
		assertThat(worker.propertyCheck(),
				containsString("Data: Key file path error"));
	}

	@Test
	public void test3() throws IOException {
		filePath = System.getProperty("user.dir") + "/key.txt";
		Assume.assumeTrue(new File(filePath).isFile());
		String data = new String(Files.readAllBytes(Paths.get(filePath)),
				StandardCharsets.UTF_8);

		when(properties.getProperty(anyString(), anyString())).thenReturn(".");
		worker = new Worker(properties);
		assertThat(worker.propertyCheck(), containsString("Data: " + data));
	}

}
