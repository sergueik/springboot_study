
package example.serializer;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.equalTo;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.avro.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;

class AvroJsonGeneratorTest {

	private AvroJsonGenerator generator;
	private ObjectMapper mapper;

	@BeforeEach
	void setUp() {
		generator = new AvroJsonGenerator();
		mapper = new ObjectMapper();
	}

	@Test
	void shouldGenerateStringField() throws IOException {
		JsonNode node = generate("schemas/string_field.avsc");
		assertThat(node.get("name").asText(), equalTo("example_string"));
	}

	@Test
	void shouldGenerateIntField() throws IOException {
		JsonNode node = generate("schemas/int_field.avsc");
		assertThat(node.get("count").asInt(), equalTo(0));
	}

	@Test
	void shouldGenerateBooleanField() throws IOException {
		JsonNode node = generate("schemas/boolean_field.avsc");
		assertThat(node.get("active").asBoolean(), is(false));
	}

	@Test
	void shouldGenerateEnumField() throws IOException {
		JsonNode node = generate("schemas/enum_field.avsc");
		assertThat(node.get("status").asText(), equalTo("ACTIVE"));
	}

	@Test
	void shouldGenerateNullableUnionInAvroJsonEncoding() throws IOException {
		JsonNode node = generate("schemas/union_field.avsc");
		// Avro JSON encoding wraps union values: {"string": "example_string"}
		assertThat(node.get("nickname").isObject(), is(true));
		assertThat(node.get("nickname").get("string").asText(), equalTo("example_string"));
	}

	@Test
	void shouldGenerateNestedRecord() throws IOException {
		JsonNode node = generate("schemas/nested_record_field.avsc");
		assertThat(node.get("inner"), notNullValue());
		assertThat(node.get("inner").get("value").asInt(), equalTo(0));
	}

	@Test
	void shouldGenerateArrayWithOneElement() throws IOException {
		JsonNode node = generate("schemas/array_field.avsc");
		assertThat(node.get("items").isArray(), is(true));
		assertThat(node.get("items").size(), is(1));
		assertThat(node.get("items").get(0).asText(), equalTo("example_string"));
	}

	@Test
	void shouldGenerateUuidLogicalType() throws IOException {
		JsonNode node = generate("schemas/uuid_field.avsc");
		String uuid = node.get("id").asText();
		assertThat(uuid, matchesPattern("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"));
	}

	@Test
	void shouldGenerateTimestampLogicalType() throws IOException {
		JsonNode node = generate("schemas/timestamp_field.avsc");
		assertThat(node.get("createdAt").asLong(), greaterThan(0L));
	}

	@Test
	void shouldGenerateLongAndDoubleFields() throws IOException {
		JsonNode node = generate("schemas/long_field.avsc");
		assertThat(node.get("views").asLong(), equalTo(0L));
		assertThat(node.get("rating").asDouble(), equalTo(0.0));
		assertThat(node.get("score").asDouble(), equalTo(0.0));
	}

	private Schema loadSchema(String resource) throws IOException {
		try (java.io.InputStream is = getClass().getClassLoader().getResourceAsStream(resource)) {
			return new Schema.Parser().parse(is);
		}
	}

	private JsonNode generate(String schemaResource) throws IOException {
		Schema schema = loadSchema(schemaResource);
		String json = generator.generate(schema);
		return mapper.readTree(json);
	}
}
