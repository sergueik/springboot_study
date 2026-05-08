package example.controller;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import com.jayway.jsonpath.JsonPath;

@SpringBootTest
@AutoConfigureMockMvc
public class AssemblyControllerTest {

	@Autowired
	private MockMvc mvc;

	private ResultActions assembly;
	private ResultActions components;
	private String json = null;
	final static String charset = "UTF-8";

	@BeforeEach
	public void beforeTest() throws Exception {
		assembly = mvc.perform(get("/assembly/transaction").accept(MediaType.APPLICATION_JSON));
		components = mvc.perform(get("/components/transaction.json").accept(MediaType.APPLICATION_JSON));
	}

	static Stream<Arguments> samples() {
		return Stream.of(Arguments.of("$.customer.accountNumber", "000111222"),
				Arguments.of("$.account.currency", "USD"), Arguments.of("$.transactionId", "T-9001"));
	}

	static Stream<Arguments> tokens() {
		return Stream.of(Arguments.of("$ref"), Arguments.of("anyOf"));
	}

	@DisplayName("Should Assemble Transaction JSON")
	@ParameterizedTest
	@MethodSource("samples")
	public void test2(String jsonPath, String value) throws Exception {

		json = assembly.andReturn().getResponse().getContentAsString();
		Object actual = JsonPath.read(json, jsonPath);
		assertThat(actual, is(value));

	}

	@DisplayName("Should Not show JSON Schema Tokens")
	@ParameterizedTest
	@MethodSource("tokens")
	public void test1(String token) throws Exception {
		json = assembly.andReturn().getResponse().getContentAsString();
		assertThat(json, not(containsString(token)));
	}

	@DisplayName("Should Allow JSON Schema Tokens")
	@ParameterizedTest
	@MethodSource("tokens")
	public void test2(String token) throws Exception {
		json = components.andReturn().getResponse().getContentAsString();
		assertThat(json, containsString(token));
	}
}
