package example.controller;

/**
 *	 Copyright 2021 Serguei Kouzmine
 */
import org.mockito.Mockito;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.mock.mockito.MockBean;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import example.controller.ExampleController;
import example.controller.ExampleController.Data;
import example.service.ExampleService;

import static org.mockito.Mockito.when;
import static org.mockito.Mockito.any;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.google.gson.Gson;

// https://stackoverflow.com/questions/61088682/is-there-any-special-configuration-to-use-springrunner-with-junit5
// https://www.baeldung.com/java-spring-mockito-mock-mockbean
@SpringBootTest
public class ServiceMockBeanUnitTest {

	@Autowired
	ApplicationContext context;

	ExampleController controller;

	@MockBean
	ExampleService mockService;

	final static String body = "Hello mock";
	final Data data = new Data("data");

	private static final Gson gson = new Gson();

	@BeforeEach
	public void setup() {

		when(mockService.hello()).thenReturn(body);
		when(mockService.handleData(any(Data.class))).thenReturn(data);
		when(mockService.handleData(any(Object.class))).thenReturn(data);
		// controller = new ExampleController(mockService);
		// NOTE: when initilizing controller through no-argument constructor test
		// observeds NPE when valudating hello() response

		// NOTE: Cannot initialize directly when using context constructor:
		// org.springframework.beans.factory.BeanCreationException:
		// Could not inject field:
		// nested exception is java.lang.IllegalStateException: The field
		// mockService cannot have an existing value
		controller = context.getBean(ExampleController.class);
	}

	@Test
	public void test1() {
		assertThat(controller, notNullValue());
	}

	@Test
	public void test2() {
		assertThat(controller.hello(), is(body));
	}

	@Test
	public void test3() {
		assertThat(controller.postJson(data), is(data));
	}

	@Test
	// returns the passed argument ?
	public void test4() {
		assertThat(controller.postJson(null), nullValue());
	}

}
