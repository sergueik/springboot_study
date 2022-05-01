package example.utils;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import example.entity.Host;

public class ClusterConfigReaderTest {

	private ClusterConfigReader snakeYamlReader = new ClusterConfigReader();
	Map<String, Host> info = new HashMap<>();
	String fileName = "cluster.yaml";
	String encoding = "UTF-8";

	@Test
	public void test() throws Exception {
		snakeYamlReader.read(fileName);
		info = snakeYamlReader.getInfo();
		assertThat(info, notNullValue());
		assertThat(info.keySet().size(), is(5));
	}
}
