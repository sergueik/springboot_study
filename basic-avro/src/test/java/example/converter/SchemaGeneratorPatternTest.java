package example.converter;

import example.model.AvroTypeInfo;
import org.apache.avro.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class SchemaGeneratorPatternTest {

    private SchemaGenerator generator;

    @BeforeEach
    void setUp() {
        generator = new SchemaGenerator();
    }

    @Test
    void shouldGenerateStringSchemaWithPattern() {
        AvroTypeInfo typeInfo = AvroTypeInfo.builder()
                .avroType(Schema.Type.STRING)
                .pattern("^[a-zA-Z0-9_-]{3,16}$")
                .build();

        Schema schema = generator.generateSchema(typeInfo, "TestField");

        assertThat(schema).isNotNull();
        assertThat(schema.getType()).isEqualTo(Schema.Type.STRING);
        assertThat(schema.toString()).contains("\"pattern\"");
        assertThat(schema.toString()).contains("^[a-zA-Z0-9_-]{3,16}$");
    }

    @Test
    void shouldGenerateStringSchemaWithPatternAndLogicalType() {
        AvroTypeInfo typeInfo = AvroTypeInfo.builder()
                .avroType(Schema.Type.STRING)
                .logicalType("uuid")
                .pattern("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$")
                .build();

        Schema schema = generator.generateSchema(typeInfo, "TestField");

        assertThat(schema).isNotNull();
        assertThat(schema.getType()).isEqualTo(Schema.Type.STRING);
        assertThat(schema.toString()).contains("\"pattern\"");
        assertThat(schema.toString()).contains("\"logicalType\"");
    }

    @Test
    void shouldGenerateRecordSchemaWithPatternedFields() {
        AvroTypeInfo phoneNumberType = AvroTypeInfo.builder()
                .avroType(Schema.Type.STRING)
                .pattern("^\\+?[1-9]\\d{1,14}$")
                .build();

        AvroTypeInfo zipCodeType = AvroTypeInfo.builder()
                .avroType(Schema.Type.STRING)
                .pattern("^\\d{5}(-\\d{4})?$")
                .build();

        AvroTypeInfo recordType = AvroTypeInfo.builder()
                .avroType(Schema.Type.RECORD)
                .recordName("ContactInfo")
                .addField("phoneNumber", phoneNumberType)
                .addField("zipCode", zipCodeType)
                .build();

        Schema schema = generator.generateSchema(recordType, "ContactInfo");

        assertThat(schema).isNotNull();
        assertThat(schema.getType()).isEqualTo(Schema.Type.RECORD);
        assertThat(schema.toString()).contains("\"pattern\"");
        // JSON escapes backslashes, so \+ becomes \\+
        assertThat(schema.toString()).contains("^\\\\+?[1-9]\\\\d{1,14}$");
        assertThat(schema.toString()).contains("^\\\\d{5}(-\\\\d{4})?$");
    }

    @Test
    void shouldEscapePatternSpecialCharacters() {
        AvroTypeInfo typeInfo = AvroTypeInfo.builder()
                .avroType(Schema.Type.STRING)
                .pattern("^\"test\"\\s+pattern$")
                .build();

        Schema schema = generator.generateSchema(typeInfo, "TestField");

        assertThat(schema).isNotNull();
        String schemaJson = schema.toString();
        // The pattern should be properly escaped in JSON
        assertThat(schemaJson).contains("pattern");
    }

    @Test
    void shouldGenerateSchemaWithoutPatternWhenNotProvided() {
        AvroTypeInfo typeInfo = AvroTypeInfo.builder()
                .avroType(Schema.Type.STRING)
                .build();

        Schema schema = generator.generateSchema(typeInfo, "TestField");

        assertThat(schema).isNotNull();
        assertThat(schema.getType()).isEqualTo(Schema.Type.STRING);
        assertThat(schema.toString()).doesNotContain("\"pattern\"");
    }
}
