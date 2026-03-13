package example.serializer;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.apache.avro.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.assertj.core.api.Assertions.assertThat;

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
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "name", "type": "string"}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        assertThat(node.get("name").asText()).isEqualTo("example_string");
    }

    @Test
    void shouldGenerateIntField() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "count", "type": "int"}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        assertThat(node.get("count").asInt()).isEqualTo(0);
    }

    @Test
    void shouldGenerateBooleanField() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "active", "type": "boolean"}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        assertThat(node.get("active").asBoolean()).isFalse();
    }

    @Test
    void shouldGenerateEnumField() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "status", "type": {"type": "enum", "name": "Status", "symbols": ["ACTIVE", "INACTIVE"]}}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        assertThat(node.get("status").asText()).isEqualTo("ACTIVE");
    }

    @Test
    void shouldGenerateNullableUnionInAvroJsonEncoding() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "nickname", "type": ["null", "string"], "default": null}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        // Avro JSON encoding wraps union values: {"string": "example_string"}
        assertThat(node.get("nickname").isObject()).isTrue();
        assertThat(node.get("nickname").get("string").asText()).isEqualTo("example_string");
    }

    @Test
    void shouldGenerateNestedRecord() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Outer",
                  "fields": [
                    {
                      "name": "inner",
                      "type": {
                        "type": "record",
                        "name": "Inner",
                        "fields": [
                          {"name": "value", "type": "int"}
                        ]
                      }
                    }
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        assertThat(node.get("inner")).isNotNull();
        assertThat(node.get("inner").get("value").asInt()).isEqualTo(0);
    }

    @Test
    void shouldGenerateArrayWithOneElement() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "items", "type": {"type": "array", "items": "string"}}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        assertThat(node.get("items").isArray()).isTrue();
        assertThat(node.get("items")).hasSize(1);
        assertThat(node.get("items").get(0).asText()).isEqualTo("example_string");
    }

    @Test
    void shouldGenerateUuidLogicalType() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "id", "type": {"type": "string", "logicalType": "uuid"}}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        String uuid = node.get("id").asText();
        assertThat(uuid).matches("[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}");
    }

    @Test
    void shouldGenerateTimestampLogicalType() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "createdAt", "type": {"type": "long", "logicalType": "timestamp-millis"}}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        assertThat(node.get("createdAt").asLong()).isGreaterThan(0);
    }

    @Test
    void shouldGenerateLongAndDoubleFields() throws IOException {
        Schema schema = new Schema.Parser().parse("""
                {
                  "type": "record",
                  "name": "Test",
                  "fields": [
                    {"name": "views", "type": "long"},
                    {"name": "rating", "type": "double"},
                    {"name": "score", "type": "float"}
                  ]
                }
                """);

        String json = generator.generate(schema);
        JsonNode node = mapper.readTree(json);

        assertThat(node.get("views").asLong()).isEqualTo(0L);
        assertThat(node.get("rating").asDouble()).isEqualTo(0.0);
        assertThat(node.get("score").asDouble()).isEqualTo(0.0);
    }
}
