package example.mapper;

import example.model.AvroTypeInfo;
import example.parser.OpenApiParser;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.assertj.core.api.Assertions.assertThat;

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
        assertThat(contactInfoSchema).isNotNull();

        Schema<?> phoneNumberSchema = contactInfoSchema.getProperties().get("phoneNumber");
        AvroTypeInfo phoneNumberType = mapper.mapSchema(phoneNumberSchema, "phoneNumber");

        assertThat(phoneNumberType).isNotNull();
        assertThat(phoneNumberType.getAvroType()).isEqualTo(org.apache.avro.Schema.Type.STRING);
        assertThat(phoneNumberType.getPattern()).isEqualTo("^\\+?[1-9]\\d{1,14}$");
    }

    @Test
    void shouldMapStringFieldWithPatternAndLogicalType() throws IOException {
        String yamlContent = """
                openapi: 3.0.3
                info:
                  title: Test API
                  version: 1.0.0
                components:
                  schemas:
                    TestSchema:
                      type: object
                      properties:
                        customId:
                          type: string
                          format: uuid
                          pattern: '^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$'
                """;

        OpenAPI openAPI = parser.parseFromString(yamlContent);
        mapper = new OpenApiToAvroTypeMapper(openAPI);

        Schema<?> testSchema = openAPI.getComponents().getSchemas().get("TestSchema");
        Schema<?> customIdSchema = testSchema.getProperties().get("customId");
        AvroTypeInfo customIdType = mapper.mapSchema(customIdSchema, "customId");

        assertThat(customIdType).isNotNull();
        assertThat(customIdType.getAvroType()).isEqualTo(org.apache.avro.Schema.Type.STRING);
        assertThat(customIdType.getLogicalType()).isEqualTo("uuid");
        assertThat(customIdType.getPattern()).isEqualTo("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$");
    }

    @Test
    void shouldMapStringFieldWithoutPattern() throws IOException {
        String yamlContent = """
                openapi: 3.0.3
                info:
                  title: Test API
                  version: 1.0.0
                components:
                  schemas:
                    TestSchema:
                      type: object
                      properties:
                        simpleString:
                          type: string
                """;

        OpenAPI openAPI = parser.parseFromString(yamlContent);
        mapper = new OpenApiToAvroTypeMapper(openAPI);

        Schema<?> testSchema = openAPI.getComponents().getSchemas().get("TestSchema");
        Schema<?> simpleStringSchema = testSchema.getProperties().get("simpleString");
        AvroTypeInfo simpleStringType = mapper.mapSchema(simpleStringSchema, "simpleString");

        assertThat(simpleStringType).isNotNull();
        assertThat(simpleStringType.getAvroType()).isEqualTo(org.apache.avro.Schema.Type.STRING);
        assertThat(simpleStringType.getPattern()).isNull();
    }

    @Test
    void shouldMapMultipleFieldsWithDifferentPatterns() throws IOException {
        OpenAPI openAPI = parser.parse("test-openapi.yaml");
        mapper = new OpenApiToAvroTypeMapper(openAPI);

        Schema<?> contactInfoSchema = openAPI.getComponents().getSchemas().get("ContactInfo");

        // Test phoneNumber
        Schema<?> phoneNumberSchema = contactInfoSchema.getProperties().get("phoneNumber");
        AvroTypeInfo phoneNumberType = mapper.mapSchema(phoneNumberSchema, "phoneNumber");
        assertThat(phoneNumberType.getPattern()).isEqualTo("^\\+?[1-9]\\d{1,14}$");

        // Test zipCode
        Schema<?> zipCodeSchema = contactInfoSchema.getProperties().get("zipCode");
        AvroTypeInfo zipCodeType = mapper.mapSchema(zipCodeSchema, "zipCode");
        assertThat(zipCodeType.getPattern()).isEqualTo("^\\d{5}(-\\d{4})?$");

        // Test username
        Schema<?> usernameSchema = contactInfoSchema.getProperties().get("username");
        AvroTypeInfo usernameType = mapper.mapSchema(usernameSchema, "username");
        assertThat(usernameType.getPattern()).isEqualTo("^[a-zA-Z0-9_-]{3,16}$");
    }

}
