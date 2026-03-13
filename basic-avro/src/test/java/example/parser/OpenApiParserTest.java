package example.parser;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.media.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.io.IOException;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

class OpenApiParserTest {

    private OpenApiParser parser;

    @BeforeEach
    void setUp() {
        parser = new OpenApiParser();
    }

    @Test
    void shouldParseValidYamlFile() throws IOException {
        OpenAPI openAPI = parser.parse("test-openapi.yaml");

        assertThat(openAPI).isNotNull();
        assertThat(openAPI.getInfo()).isNotNull();
        assertThat(openAPI.getInfo().getTitle()).isEqualTo("Test API - Complete Type Coverage");
        assertThat(openAPI.getInfo().getVersion()).isEqualTo("1.0.0");
    }

    @Test
    void shouldParseComponentSchemas() throws IOException {
        OpenAPI openAPI = parser.parse("test-openapi.yaml");

        assertThat(openAPI.getComponents()).isNotNull();
        assertThat(openAPI.getComponents().getSchemas()).isNotEmpty();
        assertThat(openAPI.getComponents().getSchemas()).containsKeys(
                "CardType", "UserStatus", "Name", "ContactInfo", "Statistics", "UserFlags",
                "Timestamps", "Coordinates", "Location", "CreditCard",
                "Job", "User", "ResultResponse"
        );
    }

    @Test
    void shouldParseEnumSchema() throws IOException {
        OpenAPI openAPI = parser.parse("test-openapi.yaml");

        Schema<?> cardTypeSchema = openAPI.getComponents().getSchemas().get("CardType");
        assertThat(cardTypeSchema).isNotNull();
        assertThat(cardTypeSchema.getEnum()).isNotNull();
        assertThat(cardTypeSchema.getEnum().toString()).contains("DEBIT", "CREDIT", "PREPAID");
    }

    @Test
    void shouldParseSchemaWithPattern() throws IOException {
        OpenAPI openAPI = parser.parse("test-openapi.yaml");

        Schema<?> contactInfoSchema = openAPI.getComponents().getSchemas().get("ContactInfo");
        assertThat(contactInfoSchema).isNotNull();
        assertThat(contactInfoSchema.getProperties()).isNotNull();

        Schema<?> phoneNumberSchema = contactInfoSchema.getProperties().get("phoneNumber");
        assertThat(phoneNumberSchema).isNotNull();
        assertThat(phoneNumberSchema.getPattern()).isEqualTo("^\\+?[1-9]\\d{1,14}$");

        Schema<?> zipCodeSchema = contactInfoSchema.getProperties().get("zipCode");
        assertThat(zipCodeSchema).isNotNull();
        assertThat(zipCodeSchema.getPattern()).isEqualTo("^\\d{5}(-\\d{4})?$");

        Schema<?> usernameSchema = contactInfoSchema.getProperties().get("username");
        assertThat(usernameSchema).isNotNull();
        assertThat(usernameSchema.getPattern()).isEqualTo("^[a-zA-Z0-9_-]{3,16}$");
    }

    @Test
    void shouldThrowExceptionForNonExistentFile() {
        assertThatThrownBy(() -> parser.parse("nonexistent.yaml"))
                .isInstanceOf(IOException.class)
                .hasMessageContaining("not found");
    }

    @Test
    void shouldThrowExceptionForInvalidContent() {
        assertThatThrownBy(() -> parser.parseFromString("invalid yaml content [[["))
                .isInstanceOf(IOException.class);
    }

    @Test
    void shouldThrowExceptionForNullContent() {
        assertThatThrownBy(() -> parser.parseFromString(null))
                .isInstanceOf(IOException.class)
                .hasMessageContaining("null or empty");
    }

    @Test
    void shouldThrowExceptionForEmptyContent() {
        assertThatThrownBy(() -> parser.parseFromString(""))
                .isInstanceOf(IOException.class)
                .hasMessageContaining("null or empty");
    }
}
