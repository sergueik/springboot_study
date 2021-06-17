package example;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.util.ArrayList;
import java.util.List;

import org.junit.Before;
import org.junit.Test;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import example.controller.DataController;

public class DataControllerTest {

	DataController sut;

	@Before
	public void setup() {
		sut = new DataController();
	}

	@Test
	public void test1() {
		final int num_rows = 8;

		sut.setDebug(true);
		ResponseEntity<List<String>> data = new ResponseEntity<List<String>>(
				new ArrayList<>(), HttpStatus.BAD_REQUEST);
		String file = "hosts.txt";
		data = sut.showServers(file);
		assertThat(data, notNullValue());
		assertThat(data.getStatusCode(), is(HttpStatus.OK));
		assertThat(data.hasBody(), is(true));
		System.err.println("Response body: " + data.getBody());
		assertThat(data.getBody().size(), is(num_rows));
		assertThat(data.getBody(), hasItems(new String[] { "host10", "host12",
				"host13", "host1", "host2", "host3", "host4", "host3", "hosts15" }));
	}

	@Test
	public void test2() {
		final int num_rows = 1;

		sut.setDebug(true);
		ResponseEntity<List<String>> data = new ResponseEntity<List<String>>(
				new ArrayList<>(), HttpStatus.BAD_REQUEST);
		String file = "no file.txt";
		data = sut.showServers(file);
		assertThat(data, notNullValue());
		assertThat(data.getStatusCode(), is(HttpStatus.BAD_REQUEST));
		assertThat(data.hasBody(), is(true));
		System.err.println("Response body: " + data.getBody());
		assertThat(data.getBody().size(), is(num_rows));
		assertThat(data.getBody().get(0),
				containsString(String.format("inaccessible file: %s", file)));
	}
}
