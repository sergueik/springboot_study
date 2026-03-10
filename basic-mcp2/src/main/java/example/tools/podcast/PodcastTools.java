package example.tools.podcast;

import example.tools.podcast.model.Episode;
import example.tools.podcast.model.PodcastStats;
import example.tools.podcast.model.Show;
import org.springaicommunity.mcp.annotation.McpTool;
import org.springaicommunity.mcp.annotation.McpToolParam;
import org.springframework.boot.autoconfigure.condition.ConditionalOnBean;
import org.springframework.stereotype.Component;

import java.util.List;

/**
 * MCP tools for podcast operations via Transistor.fm
 */
@Component
@ConditionalOnBean(PodcastService.class)
public class PodcastTools {

    private final PodcastService podcastService;

    public PodcastTools(PodcastService podcastService) {
        this.podcastService = podcastService;
    }

    @McpTool(name = "podcast-get-shows",
             description = "Get all podcast shows hosted by Dan Vega on Transistor.fm")
    public List<Show> getShows() {
        return podcastService.getAllShows();
    }

    @McpTool(name = "podcast-get-latest-episodes",
             description = "Get the most recent podcast episodes across all shows or filtered by show name/ID. " +
                          "Accepts show names like 'Spring Office Hours' or 'Fundamentals of Software Engineering'")
    public List<Episode> getLatestEpisodes(
            @McpToolParam(description = "Number of episodes to retrieve (default: 10, max: 50)",
                         required = false) String count,
            @McpToolParam(description = "Filter by show name ('Spring Office Hours', 'Fundamentals of Software Engineering') or show ID",
                         required = false) String show) {

        int maxResults = parseCount(count, 10, 50);
        return podcastService.getLatestEpisodes(maxResults, show);
    }

    @McpTool(name = "podcast-search-episodes",
             description = "Search for podcast episodes by keyword in title or description. " +
                          "Optionally filter by show name/ID (e.g., 'spring', 'java', 'testing')")
    public List<Episode> searchEpisodes(
            @McpToolParam(description = "Keyword to search for in episode titles and descriptions",
                         required = true) String keyword,
            @McpToolParam(description = "Number of episodes to retrieve (default: 10, max: 50)",
                         required = false) String count,
            @McpToolParam(description = "Filter by show name ('Spring Office Hours', 'Fundamentals of Software Engineering') or show ID",
                         required = false) String show) {

        if (keyword == null || keyword.trim().isEmpty()) {
            throw new IllegalArgumentException("Keyword parameter is required.");
        }

        int maxResults = parseCount(count, 10, 50);
        return podcastService.searchEpisodes(keyword.trim(), maxResults, show);
    }

    @McpTool(name = "podcast-get-episode-details",
             description = "Get detailed information about a specific podcast episode by its ID")
    public Episode getEpisodeDetails(
            @McpToolParam(description = "Episode ID to retrieve",
                         required = true) String episodeId) {

        if (episodeId == null || episodeId.trim().isEmpty()) {
            throw new IllegalArgumentException("Episode ID parameter is required.");
        }

        return podcastService.getEpisodeById(episodeId.trim());
    }

    @McpTool(name = "podcast-get-stats",
             description = "Get overall statistics and information about Dan Vega's podcasts, including episode counts, " +
                          "publishing frequency, and per-show summaries")
    public PodcastStats getPodcastStats() {
        return podcastService.getPodcastStats();
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
