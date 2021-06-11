package example;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import example.controller.DataController;

import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;

import java.util.ArrayList;
import java.util.List;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

public class DataControllerTest {

	private final int num_rows = 8;

	@Test
	public void test4() throws Exception {
		DataController sut = new DataController();
		sut.setDebug(true);
		List<String> data = new ArrayList<>();
		String file = "hosts.txt";
		sut.getHostDirs2(file, data);
		assertThat(data, notNullValue());
		assertThat(data.size(), is(num_rows));
		assertThat(data, hasItems(new String[] { "host10", "host12", "host13",
				"host1", "host2", "host3", "host4", "host3", "hosts15" }));
	}
}

