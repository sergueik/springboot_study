package example;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.omg.CORBA.Any;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;

import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;

import org.springframework.test.context.junit4.SpringRunner;

import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.MvcResult;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;

import example.Worker;
import example.Model;
import example.ModelMongoRepository;

import java.io.IOException;
import java.util.List;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

@RunWith(SpringRunner.class)
@WebMvcTest(Worker.class)

// based on https://github.com/bytestree/spring-restful-service-unit-test
public class WorkerTests {

	@Autowired
	private MockMvc mockMvc;

	@MockBean
	Worker worker;

	private final String URL = "/mongo/any";

	@Test
	public void test() throws Exception {
		// prepare data and mock's behaviour
		Model modelStub = new Model();
		when(worker.findOneByRepo()).thenReturn(modelStub);
		// execute
		MvcResult result = mockMvc.perform(
				MockMvcRequestBuilders.get(URL).accept(MediaType.APPLICATION_JSON_UTF8))
				.andReturn();

		// verify status is OK
		assertEquals("Incorrect Response Status", HttpStatus.OK.value(),
				result.getResponse().getStatus());
		// verify that mapped method was called
		verify(worker).findOneByRepo();

		// verify that expected type was serialized in response
		Model resultModel = jsonToObject(result.getResponse().getContentAsString(),
				Model.class);
		assertNotNull(resultModel);

	}

	@SuppressWarnings("rawtypes")
	public static List jsonToList(String json, TypeToken token) {
		Gson gson = new Gson();
		return gson.fromJson(json, token.getType());
	}

	public static String objectToJson(Object obj) {
		Gson gson = new Gson();
		return gson.toJson(obj);
	}

	public static <T> T jsonToObject(String json, Class<T> classOf) {
		Gson gson = new Gson();
		return gson.fromJson(json, classOf);
	}
}
