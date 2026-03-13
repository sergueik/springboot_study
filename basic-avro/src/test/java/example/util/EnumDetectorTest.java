package example.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class EnumDetectorTest {

    private EnumDetector detector;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        detector = new EnumDetector();
        objectMapper = new ObjectMapper();
    }

    @Test
    void shouldMatchValidEnumPatterns() {
        // Enums with underscores
        assertThat(detector.matches("STATUS_ACTIVE")).isTrue();
        assertThat(detector.matches("USER_ROLE_ADMIN")).isTrue();
        assertThat(detector.matches("TAG_PREMIUM")).isTrue();
        assertThat(detector.matches("ERROR_CODE_404")).isTrue();

        // Enums without underscores
        assertThat(detector.matches("OK")).isTrue();
        assertThat(detector.matches("SUCCESS")).isTrue();
        assertThat(detector.matches("ERROR")).isTrue();
        assertThat(detector.matches("ACTIVE")).isTrue();
        assertThat(detector.matches("PENDING")).isTrue();
        assertThat(detector.matches("FAILED")).isTrue();
    }

    @Test
    void shouldNotMatchInvalidEnumPatterns() {
        assertThat(detector.matches("lowercase")).isFalse();
        assertThat(detector.matches("MixedCase")).isFalse();
        assertThat(detector.matches("status_active")).isFalse();
        assertThat(detector.matches("_STARTS_WITH_UNDERSCORE")).isFalse();
        assertThat(detector.matches("ENDS_WITH_UNDERSCORE_")).isFalse();
        assertThat(detector.matches("A")).isFalse();
        assertThat(detector.matches("")).isFalse();
        assertThat(detector.matches(null)).isFalse();
    }

    @Test
    void shouldMatchArrayOfEnums() throws Exception {
        String json = "[\"STATUS_ACTIVE\", \"STATUS_INACTIVE\", \"STATUS_PENDING\"]";
        JsonNode arrayNode = objectMapper.readTree(json);
        assertThat(detector.matchesArray(arrayNode)).isTrue();
    }

    @Test
    void shouldMatchArrayOfEnumsWithoutUnderscores() throws Exception {
        String json = "[\"SUCCESS\", \"ERROR\", \"PENDING\", \"ACTIVE\"]";
        JsonNode arrayNode = objectMapper.readTree(json);
        assertThat(detector.matchesArray(arrayNode)).isTrue();
    }

    @Test
    void shouldNotMatchArrayWithMixedTypes() throws Exception {
        String json = "[\"STATUS_ACTIVE\", \"lowercase\"]";
        JsonNode arrayNode = objectMapper.readTree(json);
        assertThat(detector.matchesArray(arrayNode)).isFalse();
    }

    @Test
    void shouldMatchArrayWithNullsAndEnums() throws Exception {
        String json = "[\"TAG_PREMIUM\", null, \"TAG_VERIFIED\"]";
        JsonNode arrayNode = objectMapper.readTree(json);
        assertThat(detector.matchesArray(arrayNode)).isTrue();
    }

    @Test
    void shouldNotMatchEmptyArray() throws Exception {
        String json = "[]";
        JsonNode arrayNode = objectMapper.readTree(json);
        assertThat(detector.matchesArray(arrayNode)).isFalse();
    }

    @Test
    void shouldReturnNullForLogicalType() {
        assertThat(detector.getLogicalType()).isNull();
    }

    @Test
    void shouldHaveModeratePriority() {
        assertThat(detector.getPriority()).isGreaterThan(0);
        assertThat(detector.getPriority()).isLessThan(new UuidDetector().getPriority());
    }
}
