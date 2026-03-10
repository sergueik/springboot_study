package example.tools.youtube;

import example.tools.youtube.model.ChannelStats;
import example.tools.youtube.model.Video;
import org.springaicommunity.mcp.annotation.McpTool;
import org.springaicommunity.mcp.annotation.McpToolParam;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * MCP tools for YouTube channel operations
 */
@Component
@ConditionalOnBean(YouTubeService.class)
public class YouTubeTools {

    private final YouTubeService youTubeService;

    public YouTubeTools(YouTubeService youTubeService) {
        this.youTubeService = youTubeService;
    }

    @McpTool(name = "youtube-get-latest-videos",
             description = "Get the most recent videos from Dan Vega's YouTube channel")
    public List<Video> getLatestVideos(
            @McpToolParam(description = "Number of videos to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        int maxResults = parseCount(count, 10, 50);
        return youTubeService.getLatestVideos(maxResults);
    }

    @McpTool(name = "youtube-get-top-videos",
             description = "Get the top-performing videos from Dan Vega's YouTube channel by view count")
    public List<Video> getTopVideos(
            @McpToolParam(description = "Number of videos to retrieve (default: 10, max: 50)",
                         required = false) String count,
            @McpToolParam(description = "Time range: 'recent', 'month', 'year', 'all' (default: 'recent')",
                         required = false) String timeRange) {

        int maxResults = parseCount(count, 10, 50);
        String range = timeRange != null ? timeRange.toLowerCase() : "recent";
        return youTubeService.getTopVideos(maxResults, range);
    }

    @McpTool(name = "youtube-search-videos-by-topic",
             description = "Search for videos on Dan Vega's YouTube channel by topic or keyword (e.g., 'java', 'spring', 'spring-ai')")
    public List<Video> searchVideosByTopic(
            @McpToolParam(description = "Topic or keyword to search for (e.g., 'java', 'spring', 'spring-ai')",
                         required = true) String topic,
            @McpToolParam(description = "Number of videos to retrieve (default: 10, max: 50)",
                         required = false) String count) {

        if (topic == null || topic.trim().isEmpty()) {
            throw new IllegalArgumentException("Topic parameter is required.");
        }

        int maxResults = parseCount(count, 10, 50);
        return youTubeService.searchVideosByTopic(topic.trim(), maxResults);
    }

    @McpTool(name = "youtube-get-channel-stats",
             description = "Get overall statistics and information about Dan Vega's YouTube channel")
    public ChannelStats getChannelStats() {
        return youTubeService.getChannelStats();
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