package example.controller;

import java.util.Arrays;

import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.model;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.view;

import static org.hamcrest.CoreMatchers.containsString;

import example.controller.DataController;

@RunWith(SpringRunner.class)
@WebMvcTest(controllers = DataController.class)
public class DataControllerTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions resultActions;
	final static String charset = "UTF-8";
	private String variable = "hosts1";
	private String key = "datakey";

	@Before
	public void beforeTest() throws Exception {
		resultActions = mvc.perform(get(String.format("/data?name=%s&key=%s", variable, key))
				.accept(MediaType.APPLICATION_JSON_UTF8_VALUE));
	}

	@Test
	public void statusTest() throws Exception {
		resultActions.andExpect(status().isOk());
	}
}
