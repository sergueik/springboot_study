
package example;

import static com.jayway.restassured.RestAssured.*;
import static org.hamcrest.Matchers.*;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.TestPropertySource;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;
import org.springframework.test.context.web.WebAppConfiguration;
import example.App;
import com.jayway.restassured.RestAssured;
import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;

// based on: https://github.com/swathisprasad/tdd-with-springboot-restcontroller

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = App.class)
// https://github.com/aparnaittekot/Spring-Boot-App-using-JDBC
// https://github.com/in28minutes/spring-boot-examples/tree/master/spring-boot-2-jdbc-with-h2
// https://www.springboottutorial.com/spring-boot-and-spring-jdbc-with-h2
@TestPropertySource(value = { "classpath:application.properties",
		"classpath:datasource.properties" })
@SpringBootTest(webEnvironment = WebEnvironment.DEFINED_PORT)
public class GetDataTest {

	@Value("${server.port}") // 8080
	int port;

	@Test
	public void getDataTest() {
		try {
			Thread.sleep(100000);
		} catch (InterruptedException e) {
		}
		get("http://localhost:8080/public/getAllCars").then().assertThat()
				.body("data", equalTo("responseData"));
	}
	/*
		@Before
		public void setBaseUri() {
	
			RestAssured.port = 8080; // port;
			RestAssured.baseURI = "http://localhost"; // replace as appropriate
		}
	*/
}
