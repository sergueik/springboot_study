package example.mapper;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.equalTo;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import example.model.AvroTypeInfo;
import example.parser.OpenApiParser;

class OpenApiToAvroTypeMapperTest {

	private OpenApiParser parser;
	private OpenApiToAvroTypeMapper mapper;

	@BeforeEach
	void setUp() {
		parser = new OpenApiParser();
	}

	@Test
	void shouldMapStringFieldWithPattern() throws IOException {
		OpenAPI openAPI = parser.parse("test-openapi.yaml");
		mapper = new OpenApiToAvroTypeMapper(openAPI);

		Schema<?> contactInfoSchema = openAPI.getComponents().getSchemas().get("ContactInfo");
		assertThat(contactInfoSchema, notNullValue());

		Schema<?> phoneNumberSchema = contactInfoSchema.getProperties().get("phoneNumber");
		AvroTypeInfo phoneNumberType = mapper.mapSchema(phoneNumberSchema, "phoneNumber");

		assertThat(phoneNumberType, notNullValue());
		assertThat(phoneNumberType.getAvroType(), equalTo(org.apache.avro.Schema.Type.STRING));
		assertThat(phoneNumberType.getPattern(), equalTo("^\\+?[1-9]\\d{1,14}$"));
	}

	@Test
	void shouldMapStringFieldWithPatternAndLogicalType() throws IOException {
		String yamlContent = load("yaml/pattern.yaml");
		OpenAPI openAPI = parser.parseFromString(yamlContent);
		mapper = new OpenApiToAvroTypeMapper(openAPI);

		Schema<?> testSchema = openAPI.getComponents().getSchemas().get("TestSchema");
		Schema<?> customIdSchema = testSchema.getProperties().get("customId");
		AvroTypeInfo customIdType = mapper.mapSchema(customIdSchema, "customId");

		assertThat(customIdType, notNullValue());
		assertThat(customIdType.getAvroType(), equalTo(org.apache.avro.Schema.Type.STRING));
		assertThat(customIdType.getLogicalType(), equalTo("uuid"));
		assertThat(customIdType.getPattern(),
				equalTo("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$"));
	}

	@Test
	void shouldMapStringFieldWithoutPattern() throws IOException {
		String yamlContent = load("yaml/simple.yaml");

		OpenAPI openAPI = parser.parseFromString(yamlContent);
		mapper = new OpenApiToAvroTypeMapper(openAPI);

		Schema<?> testSchema = openAPI.getComponents().getSchemas().get("TestSchema");
		Schema<?> simpleStringSchema = testSchema.getProperties().get("simpleString");
		AvroTypeInfo simpleStringType = mapper.mapSchema(simpleStringSchema, "simpleString");

		assertThat(simpleStringType, notNullValue());
		assertThat(simpleStringType.getAvroType(), equalTo(org.apache.avro.Schema.Type.STRING));
		assertThat(simpleStringType.getPattern(), nullValue());
	}

	@Test
	void shouldMapMultipleFieldsWithDifferentPatterns() throws IOException {
		OpenAPI openAPI = parser.parse("test-openapi.yaml");
		mapper = new OpenApiToAvroTypeMapper(openAPI);

		Schema<?> contactInfoSchema = openAPI.getComponents().getSchemas().get("ContactInfo");

		// Test phoneNumber
		Schema<?> phoneNumberSchema = contactInfoSchema.getProperties().get("phoneNumber");
		AvroTypeInfo phoneNumberType = mapper.mapSchema(phoneNumberSchema, "phoneNumber");
		assertThat(phoneNumberType.getPattern(), equalTo("^\\+?[1-9]\\d{1,14}$"));

		// Test zipCode
		Schema<?> zipCodeSchema = contactInfoSchema.getProperties().get("zipCode");
		AvroTypeInfo zipCodeType = mapper.mapSchema(zipCodeSchema, "zipCode");
		assertThat(zipCodeType.getPattern(), equalTo("^\\d{5}(-\\d{4})?$"));

		// Test username
		Schema<?> usernameSchema = contactInfoSchema.getProperties().get("username");
		AvroTypeInfo usernameType = mapper.mapSchema(usernameSchema, "username");
		assertThat(usernameType.getPattern(), equalTo("^[a-zA-Z0-9_-]{3,16}$"));
	}

	private String load(String resource) throws IOException {
		try (java.io.InputStream is = getClass().getClassLoader().getResourceAsStream(resource)) {
			return new String(is.readAllBytes());
		}
	}
}
