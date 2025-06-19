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
import static org.hamcrest.Matchers.hasItem;
import static org.junit.Assert.assertTrue;

import java.util.List;
import java.util.Map;

import static org.hamcrest.CoreMatchers.is;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;

public class ResponseTest {
	public static String path = "users";
	public static String search = "users[0].firstName";
	public static final int statusCode = 200;
	public static String name = "Emily";
	private Response response = null;
	private String data = null;

	@Before
	public void setup() {
		RestAssured.baseURI = "https://dummyjson.com/";
		RestAssured.basePath = "";
	}

	public String result = null;

	// plain Java code with intermediate objects
	@Test
	public void test1() {
		path = "users";
		search = "users[0].firstName";
		name = "Emily";
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
		path = "users";
		search = "users[0].firstName";
		name = "Emily";
		result = RestAssured.when().get(path).then().statusCode(statusCode).extract().path(search);
		assertThat(result, containsString(name));
		System.out.println("result: " + result);
	}

	// RestArrured signature extensively builder-pattern chained code with logging
	@Test
	public void test3() {
		path = "users";
		search = "users[0].firstName";
		name = "Emily";
		result = RestAssured.when().get(path).then().statusCode(statusCode).assertThat().log().all().extract()
				.path(search);
		// NOTE: cannot invoke log() after path()
		assertThat(result, containsString(name));
		// use System.out to maintain log order
		System.out.println("result: " + result);
	}

	// https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html
	// @Ignoreresults
	@Test
	public void test4() {
		List<Map<String, ?>> results = getUsersBtEmail("emily.johnson@x.dummyjson.com");

		assertThat(results.get(0).keySet(), hasItem("firstName"));
		result = results.get(0).get("firstName").toString();
		assertThat(result, containsString(name));

		System.out.println("result: " + result);
	}

	public static List<Map<String, ?>> getUsersBtEmail(String email) {
		path = "users";

		Response response = RestAssured.given().baseUri(RestAssured.baseURI)
				.when().get(path).then().statusCode(statusCode)
				.extract().response();

		// Use GPath closure to filter
		String gpathSearch = String.format("users.findAll { it.email = '%s' }", email);
		return response.path(gpathSearch);
	}

}
