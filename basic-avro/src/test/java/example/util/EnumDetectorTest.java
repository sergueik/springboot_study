package example.util;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.JsonNode;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.equalTo;

import org.apache.avro.Schema;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;

class EnumDetectorTest {

	private EnumDetector detector;
	private ObjectMapper objectMapper;

	@BeforeEach
	void setUp() {
		detector = new EnumDetector();
		objectMapper = new ObjectMapper();
	}

	@DisplayName("should match valid enum patterns")
	@Test
	void test1() {
		// Enums with underscores
		assertThat(detector.matches("STATUS_ACTIVE"), is(true));
		assertThat(detector.matches("USER_ROLE_ADMIN"), is(true));
		assertThat(detector.matches("TAG_PREMIUM"), is(true));
		assertThat(detector.matches("ERROR_CODE_404"), is(true));

		// Enums without underscores
		assertThat(detector.matches("OK"), is(true));
		assertThat(detector.matches("SUCCESS"), is(true));
		assertThat(detector.matches("ERROR"), is(true));
		assertThat(detector.matches("ACTIVE"), is(true));
		assertThat(detector.matches("PENDING"), is(true));
		assertThat(detector.matches("FAILED"), is(true));
	}

	@DisplayName("should not match invalid enum patterns")
	@Test
	void test2() {
		assertThat(detector.matches("lowercase"), is(false));
		assertThat(detector.matches("MixedCase"), is(false));
		assertThat(detector.matches("status_active"), is(false));
		assertThat(detector.matches("_STARTS_WITH_UNDERSCORE"), is(false));
		assertThat(detector.matches("ENDS_WITH_UNDERSCORE_"), is(false));
		assertThat(detector.matches("A"), is(false));
		assertThat(detector.matches(""), is(false));
		assertThat(detector.matches(null), is(false));
	}

	@DisplayName("should match array of enums")
	@Test
	void test3() throws Exception {
		String json = "[\"STATUS_ACTIVE\", \"STATUS_INACTIVE\", \"STATUS_PENDING\"]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(true));
	}

	@DisplayName("should match array of enums without underscores")
	@Test
	void test4() throws Exception {
		String json = "[\"SUCCESS\", \"ERROR\", \"PENDING\", \"ACTIVE\"]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(true));
	}

	@DisplayName("should not match array with mixed types")
	@Test
	void test5() throws Exception {
		String json = "[\"STATUS_ACTIVE\", \"lowercase\"]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(false));
	}

	@DisplayName("should match array with null and enums")
	@Test
	void test6() throws Exception {
		String json = "[\"TAG_PREMIUM\", null, \"TAG_VERIFIED\"]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(true));
	}

	@DisplayName("should not match empty array")
	@Test
	void test7() throws Exception {
		String json = "[]";
		JsonNode arrayNode = objectMapper.readTree(json);
		assertThat(detector.matchesArray(arrayNode), is(false));
	}

	@DisplayName("should return null for logical type")
	@Test
	void test8() {
		assertThat(detector.getLogicalType(), nullValue());
	}

	@DisplayName("should have moderate priority")
	@Test
	void test9() {
		assertThat(detector.getPriority(), greaterThan(0));
		assertThat(detector.getPriority(), lessThan(new UuidDetector().getPriority()));
	}
}
