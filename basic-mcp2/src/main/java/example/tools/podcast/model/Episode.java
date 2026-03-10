package example.tools.podcast.model;

import java.time.LocalDateTime;

/**
 * Represents a podcast episode from Transistor.fm for MCP tool responses
 */
public record Episode(
        String id,
        String title,
        String description,
        String showId,
        String showTitle,
        LocalDateTime publishedAt,
        String audioUrl,
        String duration,
        String status,
        Integer season,
        Integer number
) {

    /**
     * Create a basic Episode with essential information
     */
    public static Episode basic(String id, String title, String showTitle, LocalDateTime publishedAt) {
        return new Episode(id, title, null, null, showTitle, publishedAt, null, null, null, null, null);
    }

    /**
     * Check if the episode is published
     */
    public boolean isPublished() {
        return "published".equalsIgnoreCase(status);
    }

    /**
     * Check if the episode is scheduled for future publication
     */
    public boolean isScheduled() {
        return "scheduled".equalsIgnoreCase(status);
    }

    /**
     * Get a shortened description for display
     */
    public String getShortDescription() {
        if (description == null || description.isEmpty()) {
            return "";
        }
        return description.length() > 300 ? description.substring(0, 300) + "..." : description;
    }

    /**
     * Check if episode has audio available
     */
    public boolean hasAudio() {
        return audioUrl != null && !audioUrl.isEmpty();
    }

    /**
     * Get formatted duration for display (e.g., "1h 23m")
     */
    public String getFormattedDuration() {
        if (duration == null || duration.isEmpty()) {
            return "Unknown duration";
        }
        // Duration comes as ISO 8601 duration format or seconds
        // This is a simple formatter, can be enhanced based on actual API response
        return duration;
    }

    /**
     * Get episode identifier with season and number if available
     */
    public String getEpisodeIdentifier() {
        if (season != null && number != null) {
            return String.format("S%dE%d", season, number);
        } else if (number != null) {
            return String.format("Episode %d", number);
        }
        return "";
    }
}
