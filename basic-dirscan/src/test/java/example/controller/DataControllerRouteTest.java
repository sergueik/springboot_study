package example.controller;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

import static org.hamcrest.CoreMatchers.is;
// import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.Matchers.notNullValue;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import com.jayway.jsonpath.DocumentContext;

import com.jayway.jsonpath.Criteria;
import com.jayway.jsonpath.DocumentContext;
import com.jayway.jsonpath.Filter;
import com.jayway.jsonpath.JsonPath;

import net.minidev.json.JSONArray;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = DataController.class)
public class DataControllerRouteTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	final static String charset = "UTF-8";
	private String variable = "hosts1";
	private String key = "datakey";
	private DocumentContext jsonContext;

	@Before
	public void beforeTest() throws Exception {
	}

	@Test
	public void test1() throws Exception {
		resultActions = mvc
				.perform(get(String.format("/data?name=%s&key=%s", variable, key))
						.accept(MediaType.APPLICATION_JSON_UTF8_VALUE));
		resultActions.andExpect(status().isOk());
	}

	// @Ignore
	@Test
	public void test2() throws Exception {
		resultActions = mvc
				.perform(get(String.format("/data?name=%s&key=%s", "dummy", key))
						.accept(MediaType.APPLICATION_JSON_UTF8_VALUE));
		resultActions.andExpect(status().isBadRequest());
	}

	@Test
	public void test3() throws Exception {
		resultActions = mvc.perform(get(String.format("/data/%s/%s", variable, key))
				.accept(MediaType.APPLICATION_JSON_UTF8_VALUE));
		resultActions.andExpect(status().isOk());
	}

	@Test
	public void test4() throws Exception {
		final int rows = 1;
		resultActions = mvc
				.perform(get(String.format("/typeddata/%s/%s/%d", variable, key, rows))
						.accept(MediaType.APPLICATION_JSON_UTF8_VALUE));
		resultActions.andExpect(status().isOk());
	}

	@Test
	public void test5() throws Exception {
		// make sure that there is at least 3 rows in hosts
		final int rows = 2;
		final int cnt = 3;
		final String content = mvc
				.perform(get(String.format("/typeddata/%s/%s/%d", variable, key, rows))
						.accept(MediaType.APPLICATION_JSON_UTF8_VALUE))
				.andReturn().getResponse().getContentAsString();

		assertThat(content, notNullValue());
		jsonContext = JsonPath.parse(content);
		JSONArray entries = jsonContext.read("$.*");
		assertThat(entries, notNullValue());
		assertThat(entries.size(), is(rows));
		JSONArray keys = jsonContext.read("$.*.keys()");
		assertThat(keys, notNullValue());
		System.err.println(keys.get(1).toString());
		keys = JsonPath.parse(keys.get(1).toString()).read("$.*");
		assertThat(keys.size(), is(cnt));

	}

	// test for empty
	@Test
	public void test6() throws Exception {
		final int cnt = -1;
		final String content = mvc
				.perform(get(String.format("/typeddata/%s/%s/%d", variable, key, cnt))
						.accept(MediaType.APPLICATION_JSON_UTF8_VALUE))
				.andReturn().getResponse().getContentAsString();

		assertThat(content.trim(), is("[]"));
	}
}
