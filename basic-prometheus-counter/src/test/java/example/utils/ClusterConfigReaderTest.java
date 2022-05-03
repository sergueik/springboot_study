package example.utils;

/**
 * Copyright 2022 Serguei Kouzmine
 */
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.HashMap;
import java.util.Map;

import org.junit.jupiter.api.Test;

import example.entity.Host;

public class ClusterConfigReaderTest {

	private ClusterConfigReader clusterConfigReader = new ClusterConfigReader();
	Map<String, Host> info = new HashMap<>();
	String fileName = "cluster.yaml";

	@Test
	public void test() throws Exception {
		clusterConfigReader.read(fileName);
		info = clusterConfigReader.getInfo();
		assertThat(info, notNullValue());
		assertThat(info.keySet().size(), is(5));
	}
}
