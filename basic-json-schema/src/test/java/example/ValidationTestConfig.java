package example;

import org.springframework.boot.context.properties.ConfigurationProperties;

import java.util.List;

@ConfigurationProperties(prefix = "schema-tests")
public record ValidationTestConfig(
    List<ValidationTestCase> validCases,
    List<ValidationTestCase> invalidCases
) {
    public record ValidationTestCase(
        String name,
        Boolean valid,
        String schemaResource,
        String payloadResource,
        String expectedMessage
    ) {}
}
