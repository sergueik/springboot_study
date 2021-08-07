package example;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;
import io.restassured.path.json.JsonPath;
import io.restassured.response.Response;

import io.restassured.http.ContentType;
import io.restassured.path.json.JsonPath;
import io.restassured.response.Response;
import org.junit.runners.MethodSorters;

import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertTrue;
import static org.hamcrest.CoreMatchers.is;
import org.junit.Before;
import org.junit.Test;

public class ResponseTest {
	public static final String path = "users";
	public static final String search = "data[0].name";
	public static final int statusCode = 200;
	public static final String name = "Bilva Nair";
	private Response response = null;
	private String data = null;

	@Before
	public void setup() {
		RestAssured.baseURI = "https://gorest.co.in";
		RestAssured.basePath = "public/v1";
	}

	public String result = null;

	// plain Java code with intermediate objects
	@Test
	public void test1() {
		response = RestAssured.get(path);
		assertThat(response.statusCode(), is(statusCode));
		data = response.asString();
		assertThat(data, notNullValue());
		System.out.println("response: " + data);
		result = new JsonPath(data).get(search).toString();
		assertThat(result, containsString(name));
		System.out.println("returned: " + result);
	}

	// RestArrured signature extensively builder-pattern chained code
	@Test
	public void test2() {
		result = RestAssured.when().get(path).then().statusCode(statusCode)
				.extract().path(search);
		assertThat(result, containsString(name));
		System.out.println("result: " + result);
	}

	// RestArrured signature extensively builder-pattern chained code with logging
	@Test
	public void test3() {
		result = RestAssured.when().get(path).then().statusCode(statusCode)
				.assertThat().log().all().extract().path(search);
		// NOTE: cannot invoke log() after path()
		assertThat(result, containsString(name));
		// use System.out to maintain log order
		System.out.println("result: " + result);
	}
}
