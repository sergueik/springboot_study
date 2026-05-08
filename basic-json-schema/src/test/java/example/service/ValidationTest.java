package example.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.jayway.jsonpath.JsonPath;

import com.networknt.schema.JsonSchema;
import com.networknt.schema.JsonSchemaFactory;
import com.networknt.schema.SpecVersion;
import com.networknt.schema.ValidationMessage;

import java.io.InputStream;
import java.util.Set;

import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
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

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.get;

// NOTE :needs jdk 16+
public class ValidationTest {

	private static JsonNode loadJson(String resourcePath) throws Exception {

		try (InputStream in = ValidationTest.class.getClassLoader().getResourceAsStream(resourcePath)) {
			assertThat("Missing resource: " + resourcePath, in, notNullValue());
			return mapper.readTree(in);
		}
	}

	private static final ObjectMapper mapper = new ObjectMapper();

	record TestCase(String schema, String data, boolean valid, String message) {
	}

	static Stream<TestCase> transactionCases() {
		return Stream.of(

				// expected success
				new TestCase("/schema/transaction.json", "/schema/valid/account.json", true, null),
				// expected failure
				new TestCase("/schema/transaction.json", "/schema/invalid/missing-customer-name.json", false, "name"),
				new TestCase("/schema/transaction.json", "/schema/invalid/invalid-account-type.json", false, "does not have a value"));
	}

	@ParameterizedTest(name = "[{index}] valid={2} schema={0} data={1}")
	@MethodSource("transactionCases")
	void validateTransactionSchema(TestCase testCase) throws Exception {

		JsonSchemaFactory factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V7);

		JsonSchema schema;

		try (InputStream schemaStream = getClass().getResourceAsStream(testCase.schema())) {

			assertThat("schema resource missing: " + testCase.schema(), schemaStream, notNullValue());
			JsonNode schemaNode = mapper.readTree(schemaStream);
			schema = factory.getSchema(schemaNode);
		}

		JsonNode input;

		try (InputStream dataStream = getClass().getResourceAsStream(testCase.data())) {

			assertThat("data resource missing: "  + testCase.data(), dataStream, notNullValue());

			input = mapper.readTree(dataStream);
		}

		Set<ValidationMessage> errors = schema.validate(input);

		if (testCase.valid()) {

			assertThat("Unexpected validation errors: " + errors, errors.isEmpty(), is(true));

		} else {

			assertThat("Expected validation failure", errors.isEmpty(), is(false));
			String combined = errors.stream().map(ValidationMessage::getMessage).reduce("", (a, b) -> a + "\n" + b);
			assertThat("Expected fragment not found.\n" + "Actual messages:\n" + combined,
					combined.contains(testCase.message()), is(true));
		}
	}
}
