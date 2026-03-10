package example.tools.newsletter;

import example.tools.newsletter.model.Post;
import example.tools.newsletter.model.PublicationStats;
import org.springaicommunity.mcp.annotation.McpTool;
import org.springaicommunity.mcp.annotation.McpToolParam;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * MCP tools for newsletter operations
 * Supports multiple publications (danvega, bytesizedai)
 */
@Component
@ConditionalOnBean(NewsletterService.class)
public class NewsletterTools {

    private final NewsletterService newsletterService;

    public NewsletterTools(NewsletterService newsletterService) {
        this.newsletterService = newsletterService;
    }

    @McpTool(name = "newsletter-get-latest-posts",
             description = "Get the most recent newsletter posts from Dan Vega's publications (danvega, bytesizedai, or all)")
    public List<Post> getLatestPosts(
            @McpToolParam(description = "Publication name: 'danvega', 'bytesizedai', or 'all' (default: 'all')",
                         required = false) String publication,
            @McpToolParam(description = "Number of posts to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        String pubFilter = publication != null && !publication.trim().isEmpty() ? publication.trim() : "all";
        int maxResults = parseCount(count, 10, 50);

        return newsletterService.getLatestPosts(pubFilter, maxResults);
    }

    @McpTool(name = "newsletter-search-posts-by-keyword",
             description = "Search for newsletter posts by keyword in title, content, or authors (e.g., 'spring', 'ai', 'java')")
    public List<Post> searchPostsByKeyword(
            @McpToolParam(description = "Keyword to search for in post titles, content, and authors",
                         required = true) String keyword,
            @McpToolParam(description = "Publication name: 'danvega', 'bytesizedai', or 'all' (default: 'all')",
                         required = false) String publication,
            @McpToolParam(description = "Number of posts to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        if (keyword == null || keyword.trim().isEmpty()) {
            throw new IllegalArgumentException("Keyword parameter is required.");
        }

        String pubFilter = publication != null && !publication.trim().isEmpty() ? publication.trim() : "all";
        int maxResults = parseCount(count, 10, 50);

        return newsletterService.searchPostsByKeyword(pubFilter, keyword.trim(), maxResults);
    }

    @McpTool(name = "newsletter-get-posts-by-status",
             description = "Get newsletter posts filtered by status: 'draft' (not scheduled), 'confirmed' (published/scheduled), 'archived', or 'all'")
    public List<Post> getPostsByStatus(
            @McpToolParam(description = "Post status: 'draft', 'confirmed', 'archived', or 'all' (default: 'confirmed')",
                         required = false) String status,
            @McpToolParam(description = "Publication name: 'danvega', 'bytesizedai', or 'all' (default: 'all')",
                         required = false) String publication,
            @McpToolParam(description = "Number of posts to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        String statusFilter = status != null && !status.trim().isEmpty() ? status.trim() : "confirmed";
        String pubFilter = publication != null && !publication.trim().isEmpty() ? publication.trim() : "all";
        int maxResults = parseCount(count, 10, 50);

        return newsletterService.getPostsByStatus(pubFilter, statusFilter, maxResults);
    }

    @McpTool(name = "newsletter-get-publication-stats",
             description = "Get statistics and information about Dan Vega's newsletter publications")
    public PublicationStats getPublicationStats(
            @McpToolParam(description = "Publication name: 'danvega', 'bytesizedai', or 'all' (default: 'all')",
                         required = false) String publication) {

        String pubFilter = publication != null && !publication.trim().isEmpty() ? publication.trim() : "all";
        return newsletterService.getPublicationStats(pubFilter);
    }

    /**
     * Parse count parameter with validation
     */
    private int parseCount(String count, int defaultValue, int maxValue) {
        if (count == null || count.trim().isEmpty()) {
            return defaultValue;
        }

        try {
            int parsedCount = Integer.parseInt(count.trim());
            return Math.min(Math.max(parsedCount, 1), maxValue);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }
}
