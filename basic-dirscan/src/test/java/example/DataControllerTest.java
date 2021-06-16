package example;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;

import java.util.ArrayList;
import java.util.List;

import example.controller.DataController;
import example.service.ServerService;

public class DataControllerTest {

	private final int num_rows = 8;

	@Test
	public void test4() throws Exception {
		String file = "hosts.txt";
		ServerService service = new ServerService(file);
		DataController sut = new DataController(service);
		sut.setDebug(true);
		List<String> data = new ArrayList<>();
		sut.getHostDirs2(file, data);
		assertThat(data, notNullValue());
		assertThat(data.size(), is(num_rows));
		assertThat(data, hasItems(new String[] { "host10", "host12", "host13",
				"host1", "host2", "host3", "host4", "host3", "hosts15" }));
	}

	@Test(expected = java.lang.IllegalArgumentException.class)
	public void test6() throws Exception {
		String file = "cyclic_hosts1";
		ServerService service = new ServerService(file);
		DataController sut = new DataController(service);
		sut.setDebug(true);
		List<String> data = new ArrayList<>();
		sut.getHostDirs2(file, data);
	}

	@Test
	public void test5() throws Exception {
		String file = "hosts.txt";
		ServerService service = new ServerService(file);
		DataController sut = new DataController(service);
		sut.setDebug(true);

		assertThat(sut.servers(), notNullValue());
		List<String> data = new ArrayList<>();
		data = sut.servers();
		assertThat(data, hasItems(new String[] { "host10", "host12", "host13",
				"host1", "host2", "host3", "host4", "host3", "hosts15" }));
	}
}