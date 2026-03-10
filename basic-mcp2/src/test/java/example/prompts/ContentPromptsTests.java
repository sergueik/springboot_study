package example.prompts;

import io.modelcontextprotocol.spec.McpSchema.GetPromptResult;
import io.modelcontextprotocol.spec.McpSchema.PromptMessage;
import io.modelcontextprotocol.spec.McpSchema.Role;
import io.modelcontextprotocol.spec.McpSchema.TextContent;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for ContentPrompts MCP prompt functionality
 */
class ContentPromptsTests {

    private final ContentPrompts contentPrompts = new ContentPrompts();

    @Test
    void testContentReportWithDefaultYear() {
        // When: Generate report with no arguments (defaults to current year)
        GetPromptResult result = contentPrompts.contentReport(null, null, null, null);

        // Then: Should return valid prompt result
        assertNotNull(result);
        assertNotNull(result.description());
        assertNotNull(result.messages());
        assertEquals(1, result.messages().size());

        // Verify message content
        PromptMessage message = result.messages().get(0);
        assertEquals(Role.USER, message.role());

        TextContent content = (TextContent) message.content();
        String instruction = content.text();

        // Should include current year
        assertTrue(instruction.contains("2025"), "Should default to current year 2025");

        // Should include all content types
        assertTrue(instruction.contains("YouTube Videos"));
        assertTrue(instruction.contains("Blog Posts"));
        assertTrue(instruction.contains("Newsletter Posts"));
        assertTrue(instruction.contains("Podcast Episodes"));

        // Should include CSV format instructions
        assertTrue(instruction.contains("NAME,CONTENT TYPE,EXPORT INDICATOR,DATE,CONTENT LINK,EYEBALLS LIVE,EYEBALLS POST"));
    }

    @Test
    void testContentReportWithSpecificYear() {
        // When: Generate report for specific year
        GetPromptResult result = contentPrompts.contentReport("2024", null, null, null);

        // Then: Should include specified year
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("2024"));
        assertFalse(instruction.contains("2025"));
    }

    @Test
    void testContentReportWithCustomDateRange() {
        // When: Generate report with custom date range
        GetPromptResult result = contentPrompts.contentReport(
            null,
            "2024-11-01",
            "2024-12-31",
            null
        );

        // Then: Should include custom date range
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("2024-11-01 to 2024-12-31"));

        // Should include date range in tool instructions
        assertTrue(instruction.contains("dateRange parameter: '2024-11-01 to 2024-12-31'"));
    }

    @Test
    void testContentReportWithVideoFilter() {
        // When: Filter to only video content
        GetPromptResult result = contentPrompts.contentReport(null, null, null, "video");

        // Then: Should include YouTube but not other types
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("YouTube Videos"));
        assertFalse(instruction.contains("Blog Posts:"));
        assertFalse(instruction.contains("Newsletter Posts:"));
        assertFalse(instruction.contains("Podcast Episodes:"));
    }

    @Test
    void testContentReportWithBlogFilter() {
        // When: Filter to only blog content
        GetPromptResult result = contentPrompts.contentReport(null, null, null, "blog");

        // Then: Should include blog but not other types
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("Blog Posts:"));
        assertFalse(instruction.contains("YouTube Videos:"));
        assertFalse(instruction.contains("Newsletter Posts:"));
        assertFalse(instruction.contains("Podcast Episodes:"));
    }

    @Test
    void testContentReportWithMultipleTypeFilters() {
        // When: Filter to video and blog
        GetPromptResult result = contentPrompts.contentReport(null, null, null, "video,blog");

        // Then: Should include both types
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("YouTube Videos"));
        assertTrue(instruction.contains("Blog Posts:"));
        assertFalse(instruction.contains("Newsletter Posts:"));
        assertFalse(instruction.contains("Podcast Episodes:"));
    }

    @Test
    void testContentReportWithAllTypesFilter() {
        // When: Use "all" filter
        GetPromptResult result = contentPrompts.contentReport(null, null, null, "all");

        // Then: Should include all content types
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("YouTube Videos"));
        assertTrue(instruction.contains("Blog Posts:"));
        assertTrue(instruction.contains("Newsletter Posts:"));
        assertTrue(instruction.contains("Podcast Episodes:"));
    }

    @Test
    void testContentReportIncludesCSVFormatInstructions() {
        // When: Generate any report
        GetPromptResult result = contentPrompts.contentReport(null, null, null, null);

        // Then: Should include CSV formatting instructions
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        // Check for column headers
        assertTrue(instruction.contains("NAME"));
        assertTrue(instruction.contains("CONTENT TYPE"));
        assertTrue(instruction.contains("EXPORT INDICATOR"));
        assertTrue(instruction.contains("DATE"));
        assertTrue(instruction.contains("CONTENT LINK"));
        assertTrue(instruction.contains("EYEBALLS LIVE"));
        assertTrue(instruction.contains("EYEBALLS POST"));

        // Check for content type values
        assertTrue(instruction.contains("'video'"));
        assertTrue(instruction.contains("'live stream'"));
        assertTrue(instruction.contains("'blog'"));
        assertTrue(instruction.contains("'newsletter'"));
        assertTrue(instruction.contains("'podcast'"));

        // Check for export indicator
        assertTrue(instruction.contains("export for reporting"));
    }

    @Test
    void testContentReportIncludesExampleOutput() {
        // When: Generate report
        GetPromptResult result = contentPrompts.contentReport(null, null, null, null);

        // Then: Should include example CSV output
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("Example Output Format"));
        assertTrue(instruction.contains("```csv"));
    }

    @Test
    void testContentReportIncludesSortingInstructions() {
        // When: Generate report
        GetPromptResult result = contentPrompts.contentReport(null, null, null, null);

        // Then: Should include sorting instructions
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("Sort all content by date"));
        assertTrue(instruction.contains("chronological order"));
    }

    @Test
    void testContentReportDescriptionIsInformative() {
        // When: Generate report
        GetPromptResult result = contentPrompts.contentReport("2024", null, null, null);

        // Then: Description should include the date range
        assertNotNull(result.description());
        assertTrue(result.description().contains("2024"));
        assertTrue(result.description().contains("Content Report"));
    }

    @Test
    void testContentReportWithLiveStreamFilter() {
        // When: Filter to live stream content
        GetPromptResult result = contentPrompts.contentReport(null, null, null, "live stream");

        // Then: Should include YouTube (since live streams are on YouTube)
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("YouTube Videos"));
        assertTrue(instruction.contains("'live stream'"));
    }

    @Test
    void testContentReportWithPodcastFilter() {
        // When: Filter to podcast content
        GetPromptResult result = contentPrompts.contentReport(null, null, null, "podcast");

        // Then: Should include only podcast
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("Podcast Episodes:"));
        assertFalse(instruction.contains("YouTube Videos:"));
        assertFalse(instruction.contains("Blog Posts:"));
        assertFalse(instruction.contains("Newsletter Posts:"));
    }

    @Test
    void testContentReportWithNewsletterFilter() {
        // When: Filter to newsletter content
        GetPromptResult result = contentPrompts.contentReport(null, null, null, "newsletter");

        // Then: Should include only newsletter
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("Newsletter Posts:"));
        assertFalse(instruction.contains("YouTube Videos:"));
        assertFalse(instruction.contains("Blog Posts:"));
        assertFalse(instruction.contains("Podcast Episodes:"));
    }

    @Test
    void testContentReportCustomDateRangeOverridesYear() {
        // When: Provide both year and custom date range (custom should take precedence)
        GetPromptResult result = contentPrompts.contentReport(
            "2023",
            "2024-11-01",
            "2024-12-31",
            null
        );

        // Then: Should use custom date range, not year
        assertNotNull(result);
        TextContent content = (TextContent) result.messages().get(0).content();
        String instruction = content.text();

        assertTrue(instruction.contains("2024-11-01 to 2024-12-31"));
        // The year 2023 might still appear in tool instructions, but primary range should be custom
    }
}
