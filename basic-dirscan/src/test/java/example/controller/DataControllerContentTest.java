package example.controller;

import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasSize;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.startsWith;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.header;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.jsonPath;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;

import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = DataController.class)
public class DataControllerContentTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	final static String charset = "UTF-8";
	private String hosts = "hosts1";
	private String key = "datakey";

	@Before
	public void beforeTest() throws Exception {
		resultActions = mvc
				.perform(get(String.format("/data?name=%s&key=%s", hosts, key))
						.accept(MediaType.APPLICATION_JSON_UTF8_VALUE));
	}

	// examine body
	@Test
	public void test1() throws Exception {
		resultActions.andExpect(content().string(startsWith("{")))
				.andExpect(header().string("Content-Type", is(
						MediaType.APPLICATION_JSON_UTF8_VALUE /* "application/json;charset=UTF-8" */)));
	}

	@Test
	public void test2() throws Exception {
		resultActions.andExpect(jsonPath("$.host1", is("value for host1")));
	}

	@Test
	public void test3() throws Exception {
		resultActions.andExpect(jsonPath("$[*]", hasSize(4)))
				.andExpect(jsonPath("$[*]", hasItems(new String[] { "value for host1" })))
				.andExpect(jsonPath("$.*", hasItems(new String[] { "value for host2" })));
	}

	@Ignore
	// keys() supported by jsonpath
	@Test
	public void test4() throws Exception {
		resultActions
				.andExpect(jsonPath("$.keys()", hasItems(new String[] { "host1" })));
	}
}
