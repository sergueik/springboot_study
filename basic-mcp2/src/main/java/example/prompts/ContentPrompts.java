package example.prompts;

import io.modelcontextprotocol.spec.McpSchema.GetPromptResult;
import io.modelcontextprotocol.spec.McpSchema.PromptMessage;
import io.modelcontextprotocol.spec.McpSchema.Role;
import io.modelcontextprotocol.spec.McpSchema.TextContent;
import org.springaicommunity.mcp.annotation.McpArg;
import org.springaicommunity.mcp.annotation.McpPrompt;
import org.springframework.stereotype.Component;

import java.time.LocalDate;
import java.util.List;

/**
 * MCP prompts for content reporting across all content types
 * (YouTube videos, blog posts, newsletter posts, podcast episodes)
 */
@Component
public class ContentPrompts {

    @McpPrompt(
        name = "content-report",
        description = "Generate a comprehensive content report across all content types (video, live stream, blog, newsletter, podcast) for a specified time period. Outputs CSV format with metrics."
    )
    public GetPromptResult contentReport(
            @McpArg(name = "year", description = "Year for the report (e.g., '2024', '2025'). Defaults to current year if not specified.", required = false)
            String year,
            @McpArg(name = "startDate", description = "Custom start date in YYYY-MM-DD format (e.g., '2024-01-01'). Overrides year if provided.", required = false)
            String startDate,
            @McpArg(name = "endDate", description = "Custom end date in YYYY-MM-DD format (e.g., '2024-12-31'). Overrides year if provided.", required = false)
            String endDate,
            @McpArg(name = "contentTypes", description = "Comma-separated list of content types to include: 'video', 'live stream', 'blog', 'newsletter', 'podcast', or 'all' for everything. Defaults to 'all'.", required = false)
            String contentTypes) {

        // Determine date range
        String dateRangeStr;
        if (startDate != null && endDate != null) {
            dateRangeStr = startDate + " to " + endDate;
        } else if (year != null) {
            dateRangeStr = year;
        } else {
            int currentYear = LocalDate.now().getYear();
            dateRangeStr = String.valueOf(currentYear);
        }

        // Determine content types filter
        String typesFilter = contentTypes != null && !contentTypes.trim().isEmpty()
            ? contentTypes.trim()
            : "all";

        // Build comprehensive instruction message using text blocks
        String instruction = """
            Generate a comprehensive content report for the date range: %s

            ## Instructions:

            ### 1. Data Collection
            Gather content from the following sources based on the content types filter ('%s'):

            """.formatted(dateRangeStr, typesFilter);

        // Add content type-specific sections
        if (shouldIncludeType(typesFilter, "video", "live stream")) {
            instruction += """
                **YouTube Videos:**
                - Use tool: `youtube-get-latest-videos` with a high count (e.g., 50)
                - Then use tool: `youtube-search-videos-by-topic` to find additional videos if needed
                - Filter results to the date range: %s
                - Determine if each video is a 'video' or 'live stream' (if duration or title indicates live streaming)

                """.formatted(dateRangeStr);
        }

        if (shouldIncludeType(typesFilter, "blog")) {
            instruction += """
                **Blog Posts:**
                - Use tool: `blog-get-posts-by-date-range` with dateRange parameter: '%s'
                - Retrieve up to 50 posts

                """.formatted(dateRangeStr);
        }

        if (shouldIncludeType(typesFilter, "newsletter")) {
            instruction += """
                **Newsletter Posts:**
                - Use tool: `newsletter-get-latest-posts` with publication='all' and count=50
                - Filter results to the date range: %s
                - Only include posts with status 'confirmed' (published)

                """.formatted(dateRangeStr);
        }

        if (shouldIncludeType(typesFilter, "podcast")) {
            instruction += """
                **Podcast Episodes:**
                - Use tool: `podcast-get-latest-episodes` with count=50
                - Filter results to the date range: %s

                """.formatted(dateRangeStr);
        }

        instruction += """
            ### 2. Data Formatting
            Format the output as CSV with the following columns:

            ```
            NAME,CONTENT TYPE,EXPORT INDICATOR,DATE,CONTENT LINK,EYEBALLS LIVE,EYEBALLS POST
            ```

            **Column Specifications:**
            - **NAME**: Title of the content
            - **CONTENT TYPE**: One of: 'video', 'live stream', 'blog', 'newsletter', 'podcast'
            - **EXPORT INDICATOR**: Always use 'export for reporting'
            - **DATE**: Publication date in M/D/YYYY format (e.g., 11/5/2024)
            - **CONTENT LINK**: Full URL to the content
            - **EYEBALLS LIVE**: Always 0 (live viewer metrics not currently tracked)
            - **EYEBALLS POST**:
              - For YouTube videos: Use the viewCount field
              - For all other content types: Use 0 (metrics not available via current APIs)

            ### 3. Sorting and Output
            - Sort all content by date in chronological order (oldest to newest)
            - Output the CSV with proper escaping for commas and quotes in titles
            - Include the header row

            ### 4. Example Output Format
            ```csv
            NAME,CONTENT TYPE,EXPORT INDICATOR,DATE,CONTENT LINK,EYEBALLS LIVE,EYEBALLS POST
            Spring Security 6.4 - Rest Client OAuth2 Support,video,export for reporting,11/5/2024,https://youtu.be/nFKcJDpUuZ8,0,4000
            Spring Data - Query by Example,video,export for reporting,11/8/2024,https://youtu.be/NGVWHdGNbiI,0,5000
            Spring Boot Tips and Tricks,blog,export for reporting,11/15/2024,https://www.danvega.dev/blog/spring-boot-tips,0,0
            Weekly Newsletter #45,newsletter,export for reporting,11/20/2024,https://www.danvega.dev/newsletter/45,0,0
            Spring Office Hours Episode 50,podcast,export for reporting,11/25/2024,https://www.springofficehours.io/episodes/50,0,0
            ```

            ### 5. Important Notes
            - If a tool returns an error, skip that content type and note it in a comment above the CSV
            - Handle pagination if needed to get all content in the date range
            - Ensure all dates are parsed correctly and fall within the specified range
            - Remove any duplicate entries (same content appearing multiple times)
            """;

        return new GetPromptResult(
            "Content Report Generation Instructions for " + dateRangeStr,
            List.of(new PromptMessage(Role.USER, new TextContent(instruction)))
        );
    }

    /**
     * Helper method to determine if a content type should be included based on the filter
     */
    private boolean shouldIncludeType(String filter, String... types) {
        if ("all".equalsIgnoreCase(filter)) {
            return true;
        }

        String lowerFilter = filter.toLowerCase();
        for (String type : types) {
            if (lowerFilter.contains(type.toLowerCase())) {
                return true;
            }
        }
        return false;
    }
}
