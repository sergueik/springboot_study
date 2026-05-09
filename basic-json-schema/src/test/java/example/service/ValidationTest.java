package example.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.jayway.jsonpath.JsonPath;

import com.networknt.schema.JsonSchema;
import com.networknt.schema.JsonSchemaFactory;
import com.networknt.schema.SpecVersion;
import com.networknt.schema.ValidationMessage;

import java.io.InputStream;
import java.util.HashSet;
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

	private static final ObjectMapper mapper = new ObjectMapper();
	private JsonSchemaFactory factory = null;
	private JsonSchema schema = null;
	private JsonNode schemaNode = null;
	private JsonNode input = null;
	private Set<ValidationMessage> errors = null;
	private String errorMessage = null;

	private static JsonNode loadJson(String resourcePath) throws Exception {

		try (InputStream in = ValidationTest.class.getClassLoader().getResourceAsStream(resourcePath)) {
			assertThat("Missing resource: " + resourcePath, in, notNullValue());
			return mapper.readTree(in);
		}
	}


	Stream<Arguments> transactionCases() {
		return Stream.concat(config.getValidCases().stream().map(Arguments::of),
				config.getInvalidCases().stream().map(Arguments::of));
	}


	@DisplayName("validate Transaction Schema")
	@ParameterizedTest(name = "[{index}] {0}")
	@MethodSource("transactionCases")
	void test(ValidationTestConfig.ValidationTestCase testCase) throws Exception {

		factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V7);

		try (InputStream schemaStream = getClass().getClassLoader().getResourceAsStream(testCase.getSchemaResource())) {

			assertThat("schema resource missing: " + testCase.getSchemaResource(), schemaStream, notNullValue());
			JsonNode schemaNode = mapper.readTree(schemaStream);
			schema = factory.getSchema(schemaNode);
		}

		try (InputStream dataStream = getClass().getClassLoader().getResourceAsStream(testCase.getPayloadResource())) {

			assertThat("data resource missing: " + testCase.getPayloadResource(), dataStream, notNullValue());
			input = mapper.readTree(dataStream);
		}

		errors = schema.validate(input);

		if (testCase.getValid()) {
			assertThat("Unexpected validation errors: " + errors, errors.isEmpty(), is(true));
		} else {

			assertThat("Expected validation failure", errors.isEmpty(), is(false));
			errorMessage = errors.stream().map(ValidationMessage::getMessage).reduce("", (a, b) -> a + "\n" + b);
			assertThat("Expected fragment not found.\n" + "Actual messages:\n" + errorMessage, errorMessage,
					matchesPattern("(?s).*" + testCase.getExpectedMessage() + ".*"));
		}
	}
}
