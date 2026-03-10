package example.tools.blog;

import example.Application;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.context.ApplicationContext;
import org.springframework.test.context.TestPropertySource;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration test to verify blog service and tools are properly wired in Spring context
 */
@SpringBootTest(classes = Application.class, webEnvironment = SpringBootTest.WebEnvironment.NONE)
@TestPropertySource(properties = {
    "spring.ai.anthropic.api-key=test-key",
    "dvaas.blog.rss-url=https://www.danvega.dev/rss.xml",
    "dvaas.blog.cache-duration=PT30M",
    "dvaas.speaking.api-url=https://www.danvega.dev/api/speaking",
    "dvaas.newsletter.api-key=test-newsletter-api-key",
    "dvaas.newsletter.publications.danvega=pub_test_123",
    "dvaas.podcast.api-key=test-podcast-api-key-1234567890",
    "dvaas.youtube.api-key=test-youtube-api-key-1234567890",
    "dvaas.youtube.channel-id=UC1234567890123456789012"
})
class BlogIntegrationTest {

    @Autowired(required = false)
    private ApplicationContext applicationContext;

    @Autowired(required = false)
    private BlogService blogService;

    @Autowired(required = false)
    private BlogTools blogTools;

    @Test
    void contextLoads() {
        assertNotNull(applicationContext);
    }

    @Test
    void blogService_ShouldBeCreated() {
        assertNotNull(blogService, "BlogService should be created when RSS URL is configured");
    }

    @Test
    void blogTools_ShouldBeCreated() {
        assertNotNull(blogTools, "BlogTools should be created when BlogService is available");
    }

    @Test
    void mcpToolsAreScannable() {
        // Verify that our MCP tools are discoverable by the Spring AI MCP framework
        assertTrue(applicationContext.containsBean("blogTools"));

        BlogTools tools = applicationContext.getBean("blogTools", BlogTools.class);
        assertNotNull(tools);

        // Verify the tools can be invoked (they should return collections/objects)
        assertNotNull(tools.getLatestPosts("1"));
        assertNotNull(tools.getBlogStats());
        assertNotNull(tools.searchPostsByKeyword("spring", "1"));
        assertNotNull(tools.getPostsByDateRange("2024", "1"));
    }
}