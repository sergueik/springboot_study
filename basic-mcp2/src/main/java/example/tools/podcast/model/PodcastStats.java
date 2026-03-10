package example.tools.podcast.model;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Represents overall podcast statistics for MCP tool responses
 */
public record PodcastStats(
        int totalShows,
        int totalEpisodes,
        LocalDateTime latestEpisodeDate,
        String latestEpisodeTitle,
        int episodesThisYear,
        int episodesThisMonth,
        double averageEpisodesPerMonth,
        List<ShowSummary> showSummaries
) {

    /**
     * Summary information for a single show
     */
    public record ShowSummary(
            String showTitle,
            int episodeCount,
            LocalDateTime latestEpisode
    ) {}

    /**
     * Check if there are any episodes
     */
    public boolean hasEpisodes() {
        return totalEpisodes > 0;
    }

    /**
     * Get formatted average episodes per month
     */
    public String getFormattedAverageEpisodesPerMonth() {
        return String.format("%.1f", averageEpisodesPerMonth);
    }

    /**
     * Get the most active show (by episode count)
     */
    public String getMostActiveShow() {
        if (showSummaries == null || showSummaries.isEmpty()) {
            return "N/A";
        }

        return showSummaries.stream()
                .max((s1, s2) -> Integer.compare(s1.episodeCount(), s2.episodeCount()))
                .map(ShowSummary::showTitle)
                .orElse("N/A");
    }
}
