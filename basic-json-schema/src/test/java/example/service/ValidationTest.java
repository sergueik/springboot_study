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
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
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
import static org.hamcrest.Matchers.matchesPattern;

import example.ValidationTestConfig;

// NOTE :needs jdk 16+
@SpringBootTest
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@EnableConfigurationProperties(ValidationTestConfig.class)
// @SpringBootApplication
// @ConfigurationPropertiesScan
public class ValidationTest {

	@Autowired
	ValidationTestConfig config;

	record TestCase(String schemaResource, String payloadResource, boolean valid, String expectedMessage) {
	};

	private static JsonNode loadJson(String resourcePath) throws Exception {

		try (InputStream in = ValidationTest.class.getClassLoader().getResourceAsStream(resourcePath)) {
			assertThat("Missing resource: " + resourcePath, in, notNullValue());
			return mapper.readTree(in);
		}
	}

	private static final ObjectMapper mapper = new ObjectMapper();

	Stream<Arguments> transactionCases() {
		return Stream.concat(config.validCases().stream().map(tc -> Arguments.of(tc, true)),
				config.invalidCases().stream().map(tc -> Arguments.of(tc, false)));
	}

	// @ParameterizedTest(name = "[{index}] valid={0.valid}
	// schema={0.schemaResource} data={0.payloadResource}")
	// org.junit.platform.commons.JUnitException: The display name pattern defined
	// for the parameterized test is invalid
	// caused by: java.lang.IllegalArgumentException: can't parse argument number:
	// 0.valid
	@ParameterizedTest
	@MethodSource("transactionCases")
	void validateTransactionSchema(ValidationTestConfig.ValidationTestCase testCase) throws Exception {

		JsonSchemaFactory factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V7);

		JsonSchema schema;

		try (InputStream schemaStream = getClass().getClassLoader().getResourceAsStream(testCase.schemaResource())) {

			assertThat("schema resource missing: " + testCase.schemaResource(), schemaStream, notNullValue());
			JsonNode schemaNode = mapper.readTree(schemaStream);
			schema = factory.getSchema(schemaNode);
		}

		JsonNode input;

		try (InputStream dataStream = getClass().getClassLoader().getResourceAsStream(testCase.payloadResource())) {

			assertThat("data resource missing: " + testCase.payloadResource(), dataStream, notNullValue());

			input = mapper.readTree(dataStream);
		}

		Set<ValidationMessage> errors = schema.validate(input);

		if (testCase.valid()) {

			assertThat("Unexpected validation errors: " + errors, errors.isEmpty(), is(true));

		} else {

			assertThat("Expected validation failure", errors.isEmpty(), is(false));
			String combined = errors.stream().map(ValidationMessage::getMessage).reduce("", (a, b) -> a + "\n" + b);
			assertThat("Expected fragment not found.\n" + "Actual messages:\n" + combined,
					combined, matchesPattern ("(?s).*" +  testCase.expectedMessage() + ".*"));
			// ;
		}
	}
}
