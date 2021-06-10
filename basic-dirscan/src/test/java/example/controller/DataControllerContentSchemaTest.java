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

import java.util.Map;
import java.util.Set;

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
import com.jayway.jsonpath.InvalidJsonException;
import com.jayway.jsonpath.JsonPath;

import net.minidev.json.JSONArray;
import net.minidev.json.JSONObject;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = DataController.class)
public class DataControllerContentSchemaTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	final static String charset = "UTF-8";
	private String variable = "hosts1";
	private String key = "datakey";
	private String content = null;
	private DocumentContext jsonContext = null;
	private JSONArray entries = null;
	private Set keys = null;
	private JSONArray data = null;
	private JSONArray datakeys = null;
	// make sure that there is at least 2 rows in hosts
	private int num_rows = 2;
	private int num_keys = 3;

	@Before
	public void beforeTest() throws Exception {
	}

	// matrix dimension test
	@Test
	public void test1() throws Exception {
		// make sure that there is at least 3 rows in hosts
		num_rows = 2;
		num_keys = 3;
		content = mvc
				.perform(
						get(String.format("/typeddata/%s/%s/%d", variable, key, num_rows))
								.accept(MediaType.APPLICATION_JSON_UTF8_VALUE))
				.andReturn().getResponse().getContentAsString();

		assertThat(content, notNullValue());
		jsonContext = JsonPath.parse(content);
		data = jsonContext.read("$.*.keys()");
		// matrix of keys repeated num_rows times
		assertThat(data, notNullValue());
		assertThat(data.size(), is(num_rows));
		System.err.println("data:\n" + data.get(1).toString() + "\n");
		datakeys = JsonPath.parse(data.get(1).toString()).read("$.*");
		assertThat(datakeys.size(), is(num_keys));

	}

	// matrix dimension test
	@SuppressWarnings("unchecked")
	@Test
	public void test2() throws Exception {
		// make sure that there is at least 3 rows in hosts
		num_rows = 2;
		num_keys = 3;
		content = mvc
				.perform(
						get(String.format("/typeddata/%s/%s/%d", variable, key, num_rows))
								.accept(MediaType.APPLICATION_JSON_UTF8_VALUE))
				.andReturn().getResponse().getContentAsString();

		assertThat(content, notNullValue());
		jsonContext = JsonPath.parse(content);
		entries = jsonContext.read("$[*]");
		assertThat(entries, notNullValue());
		assertThat(entries.size(), is(num_rows));
		System.err.println("entries:\n" + entries.get(1).toString() + "\n");
		// NOTE: {hostname=host2, key=datakey, value=value} - without the quotes
		try {
			keys = JsonPath
					.parse(JSONObject.toJSONString((Map<String, String>) entries.get(1)))
					.read("$.keys()");
			assertThat(keys.size(), is(num_keys));
		} catch (InvalidJsonException e) {
			System.err.println("Exception (ignored): " + e.toString());
		}
	}

	// test for empty
	@Test
	public void test3() throws Exception {
		final int num_rows = -1;
		content = mvc
				.perform(
						get(String.format("/typeddata/%s/%s/%d", variable, key, num_rows))
								.accept(MediaType.APPLICATION_JSON_UTF8_VALUE))
				.andReturn().getResponse().getContentAsString();

		assertThat(content.trim(), is("[]"));
	}
}
