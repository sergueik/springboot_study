package example.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.assertj.core.api.Assertions.assertThat;

class UuidDetectorTest {

    private UuidDetector detector;
    private ObjectMapper objectMapper;

    @BeforeEach
    void setUp() {
        detector = new UuidDetector();
        objectMapper = new ObjectMapper();
    }

    @Test
    void shouldMatchValidUuid() {
        assertThat(detector.matches("550e8400-e29b-41d4-a716-446655440000")).isTrue();
        assertThat(detector.matches("123e4567-e89b-12d3-a456-426614174000")).isTrue();
        assertThat(detector.matches("AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE")).isTrue();
    }

    @Test
    void shouldNotMatchInvalidUuid() {
        assertThat(detector.matches("not-a-uuid")).isFalse();
        assertThat(detector.matches("550e8400e29b41d4a716446655440000")).isFalse();
        assertThat(detector.matches("550e8400-e29b-41d4-a716")).isFalse();
        assertThat(detector.matches("550e8400-e29b-41d4-a716-446655440000-extra")).isFalse();
        assertThat(detector.matches("")).isFalse();
        assertThat(detector.matches(null)).isFalse();
    }

    @Test
    void shouldMatchArrayOfUuids() throws Exception {
        String json = "[\"550e8400-e29b-41d4-a716-446655440000\", \"123e4567-e89b-12d3-a456-426614174000\"]";
        JsonNode arrayNode = objectMapper.readTree(json);
        assertThat(detector.matchesArray(arrayNode)).isTrue();
    }

    @Test
    void shouldNotMatchArrayWithMixedTypes() throws Exception {
        String json = "[\"550e8400-e29b-41d4-a716-446655440000\", \"not-a-uuid\"]";
        JsonNode arrayNode = objectMapper.readTree(json);
        assertThat(detector.matchesArray(arrayNode)).isFalse();
    }

    @Test
    void shouldMatchArrayWithNullsAndUuids() throws Exception {
        String json = "[\"550e8400-e29b-41d4-a716-446655440000\", null, \"123e4567-e89b-12d3-a456-426614174000\"]";
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
    void shouldReturnCorrectLogicalType() {
        assertThat(detector.getLogicalType()).isEqualTo("uuid");
    }

    @Test
    void shouldHaveHighPriority() {
        assertThat(detector.getPriority()).isGreaterThan(0);
    }
}
