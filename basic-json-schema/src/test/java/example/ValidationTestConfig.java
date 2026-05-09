package example;

import lombok.Data;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;

@Data
@ConfigurationProperties(prefix = "schema-tests")
public class ValidationTestConfig {

	private List<ValidationTestCase> validCases;
	private List<ValidationTestCase> invalidCases;

	@Data
	public static class ValidationTestCase {

		private String name;
		private Boolean valid;

		private String schemaResource;
		private String payloadResource;

		private String expectedMessage;
	}
}