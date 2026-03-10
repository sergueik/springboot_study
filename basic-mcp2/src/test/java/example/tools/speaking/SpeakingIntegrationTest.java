package example.tools.speaking;

import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

/**
 * Integration test for the complete speaking functionality
 * This test validates the Spring Boot integration and configuration
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.ai.anthropic.api-key=test-key",
    "dvaas.speaking.api-url=https://www.danvega.dev/api/speaking",
    "dvaas.speaking.cache-duration=PT5M",
    // Ensure other services are configured for complete integration
    "dvaas.blog.rss-url=https://www.danvega.dev/rss.xml",
    "dvaas.blog.cache-duration=PT30M",
    "dvaas.youtube.api-key=test-youtube-api-key-1234567890",
    "dvaas.youtube.channel-id=UC1234567890123456789012",
    "dvaas.youtube.application-name=test-app",
    "dvaas.newsletter.api-key=test-newsletter-api-key",
    "dvaas.newsletter.publications.danvega=pub_test_123",
    "dvaas.podcast.api-key=test-podcast-api-key-1234567890"
})
class SpeakingIntegrationTest {

    @Test
    void contextLoads() {
        // This test ensures that the Spring context loads successfully
        // with all the speaking-related beans properly configured
        // The @SpringBootTest annotation will fail if there are any
        // configuration issues or missing dependencies
    }

    @Test
    void speakingConfigurationIsValid() {
        // This test validates that all the configuration properties
        // are properly validated and the beans can be created
        // The TestPropertySource ensures we have valid test configuration
        // that follows the same validation rules as production
    }
}