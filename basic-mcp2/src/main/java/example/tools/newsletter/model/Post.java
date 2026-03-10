package example.tools.newsletter.model;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Represents a newsletter post
 */
public record Post(
        String id,
        String publicationId,
        String publicationName,
        String title,
        List<String> authors,
        String status,
        LocalDateTime publishDate,
        LocalDateTime displayedDate,
        String webUrl,
        String thumbnailUrl,
        String contentPreview,
        String platform,
        String audience,
        List<String> contentTags,
        PostStats stats
) {

    /**
     * Create a basic Post without stats
     */
    public static Post basic(
            String id,
            String publicationId,
            String publicationName,
            String title,
            List<String> authors,
            String status,
            LocalDateTime publishDate,
            String webUrl) {
        return new Post(
                id,
                publicationId,
                publicationName,
                title,
                authors,
                status,
                publishDate,
                null,
                webUrl,
                null,
                null,
                null,
                null,
                null,
                null
        );
    }

    /**
     * Check if post is published (confirmed status)
     */
    public boolean isPublished() {
        return "confirmed".equalsIgnoreCase(status);
    }

    /**
     * Check if post is draft
     */
    public boolean isDraft() {
        return "draft".equalsIgnoreCase(status);
    }

    /**
     * Check if post is archived
     */
    public boolean isArchived() {
        return "archived".equalsIgnoreCase(status);
    }

    /**
     * Get formatted author list
     */
    public String getAuthorsFormatted() {
        if (authors == null || authors.isEmpty()) {
            return "Unknown";
        }
        return String.join(", ", authors);
    }

    /**
     * Get effective publish date (use displayedDate if available, otherwise publishDate)
     */
    public LocalDateTime getEffectivePublishDate() {
        return displayedDate != null ? displayedDate : publishDate;
    }
}
