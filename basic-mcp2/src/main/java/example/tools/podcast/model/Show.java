package example.tools.podcast.model;

import java.time.LocalDateTime;

/**
 * Represents a podcast show from Transistor.fm for MCP tool responses
 */
public record Show(
        String id,
        String title,
        String description,
        String author,
        String websiteUrl,
        String artworkUrl,
        String status,
        LocalDateTime createdAt
) {

    /**
     * Create a basic Show with essential information
     */
    public static Show basic(String id, String title, String description) {
        return new Show(id, title, description, null, null, null, null, null);
    }

    /**
     * Check if the show is currently active
     */
    public boolean isActive() {
        return "published".equalsIgnoreCase(status);
    }

    /**
     * Get a shortened description for display
     */
    public String getShortDescription() {
        if (description == null || description.isEmpty()) {
            return "";
        }
        return description.length() > 200 ? description.substring(0, 200) + "..." : description;
    }

    /**
     * Check if show has artwork
     */
    public boolean hasArtwork() {
        return artworkUrl != null && !artworkUrl.isEmpty();
    }
}
