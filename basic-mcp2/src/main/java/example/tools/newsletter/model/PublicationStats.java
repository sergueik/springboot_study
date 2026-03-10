package example.tools.newsletter.model;

import java.time.LocalDateTime;

/**
 * Represents statistics for a newsletter publication
 */
public record PublicationStats(
        String publicationId,
        String name,
        int totalPosts,
        int publishedPosts,
        int draftPosts,
        long activeSubscribers,
        long freeSubscribers,
        long premiumSubscribers,
        double averageOpenRate,
        double averageClickRate,
        long totalEmailsSent,
        long totalUniqueOpens,
        long totalClicks,
        LocalDateTime createdAt
) {

    /**
     * Calculate percentage of published posts
     */
    public double getPublishedPercentage() {
        if (totalPosts == 0) return 0.0;
        return (publishedPosts * 100.0) / totalPosts;
    }

    /**
     * Calculate percentage of draft posts
     */
    public double getDraftPercentage() {
        if (totalPosts == 0) return 0.0;
        return (draftPosts * 100.0) / totalPosts;
    }

    /**
     * Calculate percentage of premium subscribers
     */
    public double getPremiumSubscriberPercentage() {
        if (activeSubscribers == 0) return 0.0;
        return (premiumSubscribers * 100.0) / activeSubscribers;
    }

    /**
     * Get engagement score (combination of open and click rates)
     */
    public double getEngagementScore() {
        return (averageOpenRate + averageClickRate) / 2.0;
    }

    /**
     * Create basic stats without detailed metrics
     */
    public static PublicationStats basic(
            String publicationId,
            String name,
            int totalPosts,
            LocalDateTime createdAt) {
        return new PublicationStats(
                publicationId,
                name,
                totalPosts,
                0,
                0,
                0L,
                0L,
                0L,
                0.0,
                0.0,
                0L,
                0L,
                0L,
                createdAt
        );
    }
}
