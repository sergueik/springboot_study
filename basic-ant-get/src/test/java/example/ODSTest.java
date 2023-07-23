package example;

import org.junit.Test;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.CoreMatchers.is;

import example.ODS;

import java.io.File;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;

import static org.hamcrest.MatcherAssert.assertThat;

public class ODSTest {
	@Test
	public void fromFile() throws Exception {
		byte[] bytes = Files.readAllBytes(Paths
				.get(getClass().getClassLoader().getResource("testdata.ods").toURI()));
		assertThat(new ODS(bytes), notNullValue());
	}

}
