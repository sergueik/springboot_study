package example;

import io.restassured.RestAssured;
import io.restassured.http.ContentType;

import com.jayway.jsonpath.JsonPath;
import com.jayway.jsonpath.Configuration;
import com.jayway.jsonpath.Option;

import io.restassured.response.Response;

import io.restassured.http.ContentType;
// GPath itself is not a class, but rather a path expression language integrated into Groovy.

import groovy.xml.slurpersupport.GPathResult;
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
	private static String email = "emily.johnson@x.dummyjson.com";
	public static String name = "Emily";
	private Response response = null;
	private String data = null;
	public String result = null;

	@Before
	public void setup() {
		RestAssured.baseURI = "https://dummyjson.com/";
		RestAssured.basePath = "";
	}


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
		// System.out.println("response: " + data);
		result = JsonPath.parse(data).read(search).toString();
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
		// NOTE: log() may be quite verbose - suppress unless needed for debugging
		// result =
		// RestAssured.when().get(path).then().statusCode(statusCode).assertThat().log().all().extract().path(search);
		result = RestAssured.when().get(path).then().statusCode(statusCode).assertThat().extract().path(search);
		// NOTE: cannot invoke log() after path()
		assertThat(result, containsString(name));
		// use System.out to maintain log order
		System.out.println("result: " + result);
	}

	@Test
	public void test4() {

		response = RestAssured.given().baseUri(RestAssured.baseURI).when().get(path).then().statusCode(statusCode)
				.extract().response();
		response = RestAssured.get(path);
		// search through GPath
		search = String.format("users.findAll { it.email = '%s' }", email);
		List<Map<String, ?>> results = response.path(search);
		assertThat(results.get(0).keySet(), hasItem("firstName"));
		result = results.get(0).get("firstName").toString();
		assertThat(result, containsString(name));

		System.out.println("result: " + result);
	}

	// https://support.smartbear.com/alertsite/docs/monitors/api/endpoint/jsonpath.html
	// https://www.baeldung.com/guide-to-jayway-jsonpath
	@Test
	public void test5() {
		path = "users";

		response = RestAssured.get(path);
		assertThat(response.statusCode(), is(statusCode));
		data = response.asString();
		assertThat(data, notNullValue());
		search = String.format("users[?(@.email == '%s' )].firstName", email);
		// System.out.println("response: " + data);
		result = JsonPath.parse(data).read(search).toString();
		assertThat(result, containsString(name));
		System.out.println("returned: " + result);
	}

}
