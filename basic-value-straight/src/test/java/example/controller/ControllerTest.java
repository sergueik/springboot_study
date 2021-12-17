package example.controller;

import org.mockito.Mockito;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.annotation.PropertySource;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

import example.controller.Controller;
import example.service.ExampleService;

import static org.mockito.Mockito.when;

import static org.hamcrest.Matchers.is;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;

@RunWith(SpringJUnit4ClassRunner.class)
@SpringBootTest(webEnvironment = SpringBootTest.WebEnvironment.DEFINED_PORT, properties = { "serverPort=8443" })
@PropertySource("classpath:application.properties")
public class ControllerTest {

	Controller controller;

	@Before
	public void setup() {
		controller = new Controller();
	}

	@Test
	public void test2() {
		assertThat(controller.nullValue(), nullValue());
	}

	@Test
	public void test1() {
		assertThat(controller.value(), is("Value for test"));
	}

}
