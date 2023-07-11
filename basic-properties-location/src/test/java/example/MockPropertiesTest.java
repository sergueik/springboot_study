package example;

import org.mockito.Mockito;
import org.mockito.internal.matchers.Any;
import static org.mockito.ArgumentMatchers.anyString;

import example.controller.Worker;

import static org.mockito.Mockito.when;

import java.util.Properties;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
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

	@Test
	public void test1() {
		when(properties.getProperty(anyString(), anyString())).thenReturn("/tmp");
		worker = new Worker(properties);
		assertThat(worker.propertyCheck(),
				is("Data: read error: C:\\tmp\\key.txt"));
	}

	// @Ignore
	@Test
	public void test2() {
		when(properties.getProperty(anyString(), anyString())).thenReturn(".");
		worker = new Worker(properties);
		assertThat(worker.propertyCheck(), containsString("Data: 123"));
	}

}
