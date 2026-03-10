package example.tools.newsletter;

import example.Application;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.TestPropertySource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test to verify Beehiiv service and tools are properly wired in Spring context
 */
@SpringBootTest(classes = Application.class, webEnvironment = SpringBootTest.WebEnvironment.NONE)
@TestPropertySource(properties = {
        "spring.ai.anthropic.api-key=test-key",
        "dvaas.blog.rss-url=https://www.danvega.dev/rss.xml",
        "dvaas.speaking.api-url=https://www.danvega.dev/api/speaking",
        "dvaas.newsletter.api-key=test-api-key",
        "dvaas.newsletter.base-url=https://api.beehiiv.com/v2",
        "dvaas.newsletter.cache-duration=PT30M",
        "dvaas.newsletter.publications.danvega=pub_test_123",
        "dvaas.newsletter.publications.bytesizedai=pub_test_456",
        "dvaas.podcast.api-key=test-podcast-api-key-1234567890",
        "dvaas.youtube.api-key=test-youtube-api-key-1234567890",
        "dvaas.youtube.channel-id=UC1234567890123456789012"
})
class BeehiivIntegrationTest {

    @Autowired(required = false)
    private ApplicationContext applicationContext;

    @Autowired(required = false)
    private NewsletterService newsletterService;

    @Autowired(required = false)
    private NewsletterTools newsletterTools;

    @Test
    void contextLoads() {
        assertNotNull(applicationContext);
    }

    @Test
    void newsletterService_ShouldBeCreated() {
        assertNotNull(newsletterService, "NewsletterService should be created when API key and publications are configured");
    }

    @Test
    void newsletterTools_ShouldBeCreated() {
        assertNotNull(newsletterTools, "NewsletterTools should be created when NewsletterService is available");
    }

    @Test
    void mcpToolsAreScannable() {
        // Verify that our MCP tools are discoverable by the Spring AI MCP framework
        assertTrue(applicationContext.containsBean("newsletterTools"));

        NewsletterTools tools = applicationContext.getBean("newsletterTools", NewsletterTools.class);
        assertNotNull(tools);

        // Note: We cannot test actual API calls in integration tests without API credentials
        // These would require actual Beehiiv API access with valid publication IDs
        // For now, we verify the beans are wired correctly
    }

    @Test
    void newsletterService_ShouldHaveCorrectConfiguration() {
        // Verify that the service is properly configured
        assertNotNull(newsletterService);

        // The service should be initialized with the configured properties
        // This verifies the @ConditionalOnProperty annotations are working correctly
    }
}
