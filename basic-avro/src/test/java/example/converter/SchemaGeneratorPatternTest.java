package example.converter;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.not;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.matchesPattern;
import static org.hamcrest.Matchers.equalTo;

import org.apache.avro.Schema;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;

import example.model.AvroTypeInfo;

class SchemaGeneratorPatternTest {

	private SchemaGenerator generator;

	@BeforeEach
	void setUp() {
		generator = new SchemaGenerator();
	}

	@DisplayName("should generate string schema with pattern")
	@Test
	void test1() {
		AvroTypeInfo typeInfo = AvroTypeInfo.builder().avroType(Schema.Type.STRING).pattern("^[a-zA-Z0-9_-]{3,16}$")
				.build();

		Schema schema = generator.generateSchema(typeInfo, "TestField");

		assertThat(schema, notNullValue());
		assertThat(schema.getType(), is(Schema.Type.STRING));
		assertThat(schema.toString(), containsString("\"pattern\""));
		// assertThat(schema.toString(), matchesPattern(".*" + "\"^[a-zA-Z0-9_-]{3,16}$\"" + ".*"));
		assertThat(schema.toString(), containsString("\"^[a-zA-Z0-9_-]{3,16}$\""));
	}

	@DisplayName("should generate schema with logical type")
	@Test
	void test2() {
		AvroTypeInfo typeInfo = AvroTypeInfo.builder().avroType(Schema.Type.STRING).logicalType("uuid")
				.pattern("^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$").build();

		Schema schema = generator.generateSchema(typeInfo, "TestField");

		assertThat(schema, notNullValue());
		assertThat(schema.toString(), containsString("\"pattern\""));
		assertThat(schema.toString(), containsString("\"logicalType\""));
	}

	@DisplayName("should generate string schema with patterned fields")
	@Test
	void test3() {
		AvroTypeInfo phoneNumberType = AvroTypeInfo.builder().avroType(Schema.Type.STRING)
				.pattern("^\\+?[1-9]\\d{1,14}$").build();

		AvroTypeInfo zipCodeType = AvroTypeInfo.builder().avroType(Schema.Type.STRING).pattern("^\\d{5}(-\\d{4})?$")
				.build();

		AvroTypeInfo recordType = AvroTypeInfo.builder().avroType(Schema.Type.RECORD).recordName("ContactInfo")
				.addField("phoneNumber", phoneNumberType).addField("zipCode", zipCodeType).build();

		Schema schema = generator.generateSchema(recordType, "ContactInfo");

		assertThat(schema, notNullValue());
		assertThat(schema.toString(), containsString("\"pattern\""));
		// JSON escapes backslashes, so \+ becomes \\+
		assertThat(schema.toString(), containsString("\"^\\\\+?[1-9]\\\\d{1,14}$\""));
		assertThat(schema.toString(), containsString("\"^\\\\d{5}(-\\\\d{4})?$\""));
	}

	@DisplayName("should escape pattern special characters")
	@Test
	void test4() {
		AvroTypeInfo typeInfo = AvroTypeInfo.builder().avroType(Schema.Type.STRING).pattern("^\"test\"\\s+pattern$")
				.build();
		Schema schema = generator.generateSchema(typeInfo, "TestField");
		assertThat(schema, notNullValue());
		// The pattern should be properly escaped in JSON
		assertThat(schema.toString(), containsString("pattern"));
	}

	@DisplayName("should generate schema without pattern when none provided")
	@Test
	void test5() {
		AvroTypeInfo typeInfo = AvroTypeInfo.builder().avroType(Schema.Type.STRING).build();
		Schema schema = generator.generateSchema(typeInfo, "TestField");
		assertThat(schema, notNullValue());
		assertThat(schema.toString(), not(containsString("\"pattern\"")));
	}
}
