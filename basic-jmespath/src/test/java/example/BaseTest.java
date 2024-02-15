package example;

import static org.hamcrest.CoreMatchers.is;

/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.io.IOException;

import org.junit.Before;
import org.junit.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.burt.jmespath.Expression;
import io.burt.jmespath.JmesPath;
import io.burt.jmespath.jackson.JacksonRuntime;

public class BaseTest {

	private boolean debug = false;

	public boolean getDebug() {
		return debug;
	}

	public void setDebug(boolean value) {
		debug = value;
	}

	private JmesPath<JsonNode> jmespath = null;

	Expression<JsonNode> expression = null;

	String jsonString = "{\"k1\":\"v1\",\"k2\":\"v2\"}";

	ObjectMapper mapper = new ObjectMapper();
	JsonNode result = null;
	JsonNode input = null;

	@Before
	public void beforeClass() throws IOException {
		jmespath = new JacksonRuntime();

	}

	@Test
	public void test1() throws JsonProcessingException {
		input = mapper.readTree(jsonString);
		expression = jmespath.compile("k1");
		result = expression.search(input);
		assertThat(result, notNullValue());
		assertThat(result.asText(), is("v1"));
	}
}
