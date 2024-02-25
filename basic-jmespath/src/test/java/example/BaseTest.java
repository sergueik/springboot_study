package example;

import static org.hamcrest.CoreMatchers.is;

/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.testng.annotations.BeforeClass;
import org.testng.annotations.Test;

import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import io.burt.jmespath.Expression;
import io.burt.jmespath.JmesPath;
import io.burt.jmespath.jackson.JacksonRuntime;

@SuppressWarnings("deprecation")
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

	@BeforeClass
	public void beforeClass() {
		jmespath = new JacksonRuntime();

	}

	@Test(enabled = true)
	public void test1() throws JsonProcessingException {
		input = mapper.readTree(jsonString);
		expression = jmespath.compile("k1");
		result = expression.search(input);
		assertThat(result, notNullValue());
		assertThat(result.asText(), is("v1"));
	}
}
