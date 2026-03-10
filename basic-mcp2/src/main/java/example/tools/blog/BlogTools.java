package example.tools.blog;

import example.tools.blog.model.BlogPost;
import example.tools.blog.model.BlogStats;
import org.springaicommunity.mcp.annotation.McpTool;
import org.springaicommunity.mcp.annotation.McpToolParam;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.stereotype.Component;

import java.time.LocalDateTime;
import java.time.format.DateTimeParseException;
import java.util.List;

@Component
@ConditionalOnBean(BlogService.class)
public class BlogTools {

    private final BlogService blogService;

    public BlogTools(BlogService blogService) {
        this.blogService = blogService;
    }

    @McpTool(name = "blog-get-latest-posts", description = "Get the most recent blog posts from Dan Vega's blog")
    public List<BlogPost> getLatestPosts(@McpToolParam(description = "Number of posts to retrieve (default: 10, max: 50)", required = false) String count) {
        int maxResults = parseCount(count, 10, 50);
        return blogService.getLatestPosts(maxResults);
    }

    @McpTool(name = "blog-search-posts-by-keyword", description = "Search for blog posts by keyword in title or description (e.g., 'spring boot', 'ai', 'graphql')")
    public List<BlogPost> searchPostsByKeyword(
            @McpToolParam(description = "Keyword to search for in post titles and descriptions",
                         required = true) String keyword,
            @McpToolParam(description = "Number of posts to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        if (keyword == null || keyword.trim().isEmpty()) {
            throw new IllegalArgumentException("Keyword parameter is required.");
        }

        int maxResults = parseCount(count, 10, 50);
        return blogService.searchPostsByKeyword(keyword.trim(), maxResults);
    }

    @McpTool(name = "blog-get-posts-by-date-range", description = "Get blog posts within a specific date range or year (e.g., '2024', '2023-01-01 to 2023-12-31')")
    public List<BlogPost> getPostsByDateRange(
            @McpToolParam(description = "Date range: '2024' for year, or 'YYYY-MM-DD to YYYY-MM-DD' for custom range",
                         required = true) String dateRange,
            @McpToolParam(description = "Number of posts to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        if (dateRange == null || dateRange.trim().isEmpty()) {
            throw new IllegalArgumentException("Date range parameter is required. Use format '2024' or '2023-01-01 to 2023-12-31'.");
        }

        int maxResults = parseCount(count, 10, 50);
        return parseDateRangeAndSearch(dateRange.trim(), maxResults);
    }

    @McpTool(name = "blog-get-stats", description = "Get overall statistics and information about Dan Vega's blog")
    public BlogStats getBlogStats() {
        return blogService.getBlogStats();
    }

    /**
     * Parse date range input and perform search
     */
    private List<BlogPost> parseDateRangeAndSearch(String dateRange, int maxResults) {
        // Handle year-only format (e.g., "2024")
        if (dateRange.matches("\\d{4}")) {
            int year = Integer.parseInt(dateRange);
            return blogService.getPostsByYear(year, maxResults);
        }

        // Handle range format (e.g., "2023-01-01 to 2023-12-31")
        if (dateRange.contains(" to ")) {
            String[] parts = dateRange.split(" to ");
            if (parts.length != 2) {
                throw new IllegalArgumentException("Invalid date range format. Use 'YYYY-MM-DD to YYYY-MM-DD'.");
            }

            try {
                LocalDateTime startDate = LocalDateTime.parse(parts[0].trim() + "T00:00:00");
                LocalDateTime endDate = LocalDateTime.parse(parts[1].trim() + "T23:59:59");
                return blogService.getPostsByDateRange(startDate, endDate, maxResults);
            } catch (DateTimeParseException e) {
                throw new IllegalArgumentException("Invalid date format. Use YYYY-MM-DD format.");
            }
        }

        // Handle single date format (e.g., "2023-12-25")
        if (dateRange.matches("\\d{4}-\\d{2}-\\d{2}")) {
            try {
                LocalDateTime singleDate = LocalDateTime.parse(dateRange + "T00:00:00");
                LocalDateTime endOfDay = singleDate.plusDays(1).minusSeconds(1);
                return blogService.getPostsByDateRange(singleDate, endOfDay, maxResults);
            } catch (DateTimeParseException e) {
                throw new IllegalArgumentException("Invalid date format. Use YYYY-MM-DD format.");
            }
        }

        throw new IllegalArgumentException("Invalid date range format. Use '2024' or '2023-01-01 to 2023-12-31'.");
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