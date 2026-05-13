package example.service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;

import com.jayway.jsonpath.JsonPath;

import com.networknt.schema.JsonSchema;
import com.networknt.schema.JsonSchemaFactory;
import com.networknt.schema.SpecVersion;
import com.networknt.schema.ValidationMessage;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTest;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.junit.jupiter.SpringExtension;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.MediaType;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.ResultActions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.containsString;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.notNullValue;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.greaterThanOrEqualTo;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import java.util.stream.Stream;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.provider.Arguments;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import example.ValidationTestConfig;

// Spring Boot:
//
// loads application.yml
// binds @ConfigurationProperties
// creates Binder infrastructure
// but does NOT start the web server because of NONE


// there is a subtle timing issue:
// JUnit tries to discover parameterized arguments *before* Spring injection has populated:
//
// un-commenting the next line and removing SprongBootTest leads to subtle:
// java.lang.NullPointerException: Cannot invoke "java.util.List.stream()" because 
// the return value of "example.ValidationTestConfig.validCases()" is null
// at static_config.validCases()...
// @ExtendWith(SpringExtension.class)

@SpringBootTest(
	    classes = ValidationTest.TestConfig.class,
	    webEnvironment = SpringBootTest.WebEnvironment.NONE
	)


@ContextConfiguration(classes = ValidationTest.TestConfig.class)
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class ValidationTest {

    @Configuration
    @EnableConfigurationProperties(ValidationTestConfig.class)
    static class TestConfig {
    }

    @Autowired
    ValidationTestConfig config;

    static ValidationTestConfig static_config;

    @BeforeAll
    void init() {
        static_config = config;
    }
    // NOTE: manually import properties is Very verbose and not worth it.
    static Stream<Arguments> transactionCases() {
        return Stream.concat(
            static_config.validCases().stream().map(Arguments::of),
            static_config.invalidCases().stream().map(Arguments::of)
        );
    }
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
/*
	@DisplayName("validate Transaction Schema")
	@ParameterizedTest(name = "[{index}] {0}")
	@MethodSource("transactionCases")
	void test1(ValidationTestConfig.ValidationTestCase testCase) throws Exception {

		factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V7);

		try (InputStream schemaStream = getClass().getClassLoader().getResourceAsStream(testCase.schemaResource())) {
			assertThat("schema resource missing: " + testCase.schemaResource(), schemaStream, notNullValue());
			schemaNode = mapper.readTree(schemaStream);
			schema = factory.getSchema(schemaNode);
		}

		try (InputStream dataStream = getClass().getClassLoader().getResourceAsStream(testCase.payloadResource())) {
			assertThat("data resource missing: " + testCase.payloadResource(), dataStream, notNullValue());
			input = mapper.readTree(dataStream);
		}

		errors = schema.validate(input);

		if (testCase.valid()) {
			assertThat("Unexpected validation errors: " + errors, errors.isEmpty(), is(true));
		} else {
			assertThat("Expected validation failure", errors.isEmpty(), is(false));
			errorMessage = errors.stream().map(ValidationMessage::getMessage).reduce("", (a, b) -> a + "\n" + b);
			assertThat("Expected fragment not found.\n" + "Actual messages:\n" + errorMessage, errorMessage,
					matchesPattern("(?s).*" + testCase.expectedMessage() + ".*"));
		}
	}
	
	*/
	
	@DisplayName("validate Transaction Schema When Multiple Defects Are Observed")
	@ParameterizedTest(name = "[{index}] {0}")
	@MethodSource("transactionCases")
	void test2(ValidationTestConfig.ValidationTestCase testCase) throws Exception {

		factory = JsonSchemaFactory.getInstance(SpecVersion.VersionFlag.V7);

		try (InputStream schemaStream = getClass().getClassLoader().getResourceAsStream(testCase.schemaResource())) {
			assertThat("schema resource missing: " + testCase.schemaResource(), schemaStream, notNullValue());
			schemaNode = mapper.readTree(schemaStream);
			schema = factory.getSchema(schemaNode);
		}

		try (InputStream dataStream = getClass().getClassLoader().getResourceAsStream(testCase.payloadResource())) {
			assertThat("data resource missing: " + testCase.payloadResource(), dataStream, notNullValue());
			input = mapper.readTree(dataStream);
		}

		errors = schema.validate(input);

		if (testCase.valid()) {
			assertThat("Unexpected validation errors: " + errors, errors.isEmpty(), is(true));
		} else {
			Integer minimumErrors = Math.max(1, testCase.errorCount());
	        assertThat(errors.size(), greaterThanOrEqualTo(minimumErrors));
			List<String> actualMessages =
				    errors.stream()
				          .map(ValidationMessage::getMessage).map((String s) -> s.replaceAll("(?:\\n|\\r)"," ")).collect(Collectors.toList());
			
				String errorMessage = errors.stream().map(ValidationMessage::getMessage).reduce("", (a, b) -> a + "\n" + b);

				List<String> expectedMessages = testCase.expectedMessages() == null ? new ArrayList<>():  testCase.expectedMessages();
				for (String expected : expectedMessages) {
					// TODO: m.matchesPattern("(?s).*" + expected + ".*")
				    assertThat(String.format("%s must contain error message: %s", errorMessage,  expected),
				        actualMessages.stream().anyMatch(m -> m.contains(expected)),
				        is(true)
				    );
				}
		}
	}

}
