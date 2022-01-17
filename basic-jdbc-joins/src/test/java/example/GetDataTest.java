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

import com.jayway.restassured.RestAssured;
import com.jayway.restassured.response.Response;
import com.jayway.restassured.specification.RequestSpecification;

import org.springframework.boot.test.context.SpringBootTest.WebEnvironment;

import example.App;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration(classes = App.class)
// https://github.com/aparnaittekot/Spring-Boot-App-using-JDBC
// https://github.com/in28minutes/spring-boot-examples/tree/master/spring-boot-2-jdbc-with-h2
// https://www.springboottutorial.com/spring-boot-and-spring-jdbc-with-h2
@TestPropertySource(value = { "classpath:application.properties" })
@SpringBootTest(webEnvironment = WebEnvironment.DEFINED_PORT)
public class GetDataTest {

	private final int delay = 1000;
	@Value("${server.port}") // 8080
	int port;

	@Before
	public void setup() {

		RestAssured.port = 8080; // port;
		RestAssured.baseURI = "http://localhost"; // replace as appropriate
	}

	@Test
	public void test1() {
		RequestSpecification request = RestAssured.given();

		Response response = request.get("http://localhost:8080/data");
		System.out.println("Response Body -> " + response.body().asString());
	}

	@Test
	public void test2() {
		try {
			Thread.sleep(delay);
		} catch (InterruptedException e) {
		}
		get("http://localhost:8080/data").then().assertThat().body("data",
				containsInAnyOrder("B", "S"));

		// NOTE:
		// get("http://localhost:8080/data").then().assertThat().body("data",
		// equalTo(new String[]{"B", "S"}));
		// Expected: ["B", "S"] Actual: [B, S]
	}

}

