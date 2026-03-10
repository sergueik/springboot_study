package example.tools.speaking;

import example.tools.speaking.model.SpeakingEngagement;
import example.tools.speaking.model.SpeakingSearchResult;
import example.tools.speaking.model.SpeakingStats;
import org.springaicommunity.mcp.annotation.McpTool;
import org.springaicommunity.mcp.annotation.McpToolParam;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * MCP tools for speaking engagement operations
 */
@Component
@ConditionalOnBean(SpeakingService.class)
public class SpeakingTools {

    private final SpeakingService speakingService;

    public SpeakingTools(SpeakingService speakingService) {
        this.speakingService = speakingService;
    }

    @McpTool(name = "speaking-get-latest-engagements",
             description = "Get the most recent speaking engagements from Dan Vega's speaking schedule")
    public List<SpeakingEngagement> getLatestEngagements(
            @McpToolParam(description = "Number of engagements to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        int maxResults = parseCount(count, 10, 50);
        return speakingService.getLatestEngagements(maxResults);
    }

    @McpTool(name = "speaking-get-upcoming-events",
             description = "Get upcoming speaking events from Dan Vega's speaking schedule")
    public List<SpeakingEngagement> getUpcomingEvents(
            @McpToolParam(description = "Number of events to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        int maxResults = parseCount(count, 10, 50);
        return speakingService.getUpcomingEngagements(maxResults);
    }

    @McpTool(name = "speaking-search-by-topic",
             description = "Search for speaking engagements by topic or keyword (e.g., 'spring', 'ai', 'java', 'microservices')")
    public List<SpeakingEngagement> searchByTopic(
            @McpToolParam(description = "Topic or keyword to search for in titles, descriptions, or event names",
                         required = true) String topic,
            @McpToolParam(description = "Number of engagements to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        if (topic == null || topic.trim().isEmpty()) {
            throw new IllegalArgumentException("Topic parameter is required.");
        }

        int maxResults = parseCount(count, 10, 50);
        SpeakingSearchResult searchResult = speakingService.searchEngagementsByKeyword(topic.trim(), maxResults);
        return searchResult.engagements();
    }

    @McpTool(name = "speaking-get-stats",
             description = "Get overall statistics and information about Dan Vega's speaking engagements")
    public SpeakingStats getSpeakingStats() {
        return speakingService.getSpeakingStats();
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