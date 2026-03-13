package example.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.equalTo;

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
		assertThat(detector.matches("550e8400-e29b-41d4-a716-446655440000"), is(true));
		assertThat(detector.matches("123e4567-e89b-12d3-a456-426614174000"), is(true));
		assertThat(detector.matches("AAAAAAAA-BBBB-CCCC-DDDD-EEEEEEEEEEEE"), is(true));
	}

	@Test
	void shouldNotMatchInvalidUuid() {
		assertThat(detector.matches("not-a-uuid"), is(false));
		assertThat(detector.matches("550e8400e29b41d4a716446655440000"), is(false));
		assertThat(detector.matches("550e8400-e29b-41d4-a716"), is(false));
		assertThat(detector.matches("550e8400-e29b-41d4-a716-446655440000-extra"), is(false));
		assertThat(detector.matches(""), is(false));
		assertThat(detector.matches(null), is(false));
	}

	@Test
	void shouldMatchArrayOfUuids() throws Exception {
		String json = "[\"550e8400-e29b-41d4-a716-446655440000\", \"123e4567-e89b-12d3-a456-426614174000\"]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(true));
	}

	@Test
	void shouldNotMatchArrayWithMixedTypes() throws Exception {
		String json = "[\"550e8400-e29b-41d4-a716-446655440000\", \"not-a-uuid\"]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(false));
	}

	@Test
	void shouldMatchArrayWithNullsAndUuids() throws Exception {
		String json = "[\"550e8400-e29b-41d4-a716-446655440000\", null, \"123e4567-e89b-12d3-a456-426614174000\"]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(true));
	}

	@Test
	void shouldNotMatchEmptyArray() throws Exception {
		String json = "[]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(false));
	}

	@Test
	void shouldReturnCorrectLogicalType() {
		assertThat(detector.getLogicalType(), equalTo("uuid"));
	}

	@Test
	void shouldHaveHighPriority() {
		assertThat(detector.getPriority(), greaterThan(0));
	}
}
