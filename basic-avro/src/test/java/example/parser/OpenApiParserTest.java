package example.parser;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.empty;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.containsInAnyOrder;
import static org.hamcrest.Matchers.hasItems;
import static org.hamcrest.Matchers.hasKey;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

class OpenApiParserTest {

	private OpenApiParser parser;

	@BeforeEach
	void setUp() {
		parser = new OpenApiParser();
	}

	@Test
	void shouldParseValidYamlFile() throws IOException {
		OpenAPI openAPI = parser.parse("test-openapi.yaml");

		assertThat(openAPI, notNullValue());
		assertThat(openAPI.getInfo(), notNullValue());
		assertThat(openAPI.getInfo().getTitle(), equalTo("Test API - Complete Type Coverage"));
		assertThat(openAPI.getInfo().getVersion(), equalTo("1.0.0"));
	}

	@Test
	void shouldParseComponentSchemas() throws IOException {
		OpenAPI openAPI = parser.parse("test-openapi.yaml");

		assertThat(openAPI.getComponents(), notNullValue());
		Map<String, Schema> schemas = openAPI.getComponents().getSchemas();
		assertThat(schemas.size(), is(not(equalTo(0))));
		assertThat(schemas.keySet(), is(not(empty())));
		// final Set<String> supportedKeywords = new HashSet<>();
		for (String key : Arrays
				.asList(new String[] { "CardType", "UserStatus", "Name", "ContactInfo", "Statistics", "UserFlags",
						"Timestamps", "Coordinates", "Location", "CreditCard", "Job", "User", "ResultResponse" })) {
			assertThat(schemas, hasKey(key));
		}
	}

	@Test
	void shouldParseEnumSchema() throws IOException {
		OpenAPI openAPI = parser.parse("test-openapi.yaml");

		Schema<?> cardTypeSchema = openAPI.getComponents().getSchemas().get("CardType");
		assertThat(cardTypeSchema, notNullValue());
		assertThat(cardTypeSchema.getEnum(), notNullValue());
		// assertThat(cardTypeSchema.getEnum().toString()).contains("DEBIT", "CREDIT",
		// "PREPAID");
		for (String s : Arrays.asList(new String[] { "DEBIT", "CREDIT", "PREPAID" })) {
			assertThat(cardTypeSchema.getEnum().toString(), containsString(s));
		}
	}

	@Test
	void shouldParseSchemaWithPattern() throws IOException {
		OpenAPI openAPI = parser.parse("test-openapi.yaml");

		Schema<?> contactInfoSchema = openAPI.getComponents().getSchemas().get("ContactInfo");
		assertThat(contactInfoSchema, notNullValue());
		assertThat(contactInfoSchema.getProperties(), notNullValue());

		Schema<?> phoneNumberSchema = contactInfoSchema.getProperties().get("phoneNumber");
		assertThat(phoneNumberSchema, notNullValue());
		assertThat(phoneNumberSchema.getPattern(), equalTo("^\\+?[1-9]\\d{1,14}$"));

		Schema<?> zipCodeSchema = contactInfoSchema.getProperties().get("zipCode");
		assertThat(zipCodeSchema, notNullValue());
		assertThat(zipCodeSchema.getPattern(), equalTo("^\\d{5}(-\\d{4})?$"));

		Schema<?> usernameSchema = contactInfoSchema.getProperties().get("username");
		assertThat(usernameSchema, notNullValue());
		assertThat(usernameSchema.getPattern(), equalTo("^[a-zA-Z0-9_-]{3,16}$"));
	}

	@Test
	void shouldThrowExceptionForNonExistentFile() {
		IOException e = assertThrows(IOException.class, () -> parser.parse("nonexistent.yaml"));
		assertThat(e.getMessage(), containsString("not found"));
	}

	@Test
	void shouldThrowExceptionForInvalidContent() {
		IOException exception = assertThrows(IOException.class,
				() -> parser.parseFromString("invalid yaml content [[["));
	}

	@Test
	void shouldThrowExceptionForNullContent() {
		IOException e = assertThrows(IOException.class, () -> parser.parseFromString(null));
		assertThat(e.getMessage(), containsString("null or empty"));
	}

	@Test
	void shouldThrowExceptionForEmptyContent() {
		IOException e = assertThrows(IOException.class, () -> parser.parseFromString(""));
		assertThat(e.getMessage(), containsString("null or empty"));
	}
}
