package example.tools.newsletter.model;

/**
 * Represents statistics for a newsletter post
 */
public record PostStats(
        long opens,
        long clicks,
        long uniqueOpens,
        long uniqueClicks
) {

    /**
     * Calculate open rate (percentage)
     */
    public double getOpenRate(long totalRecipients) {
        if (totalRecipients == 0) return 0.0;
        return (uniqueOpens * 100.0) / totalRecipients;
    }

    /**
     * Calculate click rate (percentage)
     */
    public double getClickRate(long totalRecipients) {
        if (totalRecipients == 0) return 0.0;
        return (uniqueClicks * 100.0) / totalRecipients;
    }

    /**
     * Calculate click-to-open rate (percentage)
     */
    public double getClickToOpenRate() {
        if (uniqueOpens == 0) return 0.0;
        return (uniqueClicks * 100.0) / uniqueOpens;
    }

    /**
     * Create empty stats
     */
    public static PostStats empty() {
        return new PostStats(0, 0, 0, 0);
    }

    /**
     * Check if stats are available
     */
    public boolean hasStats() {
        return opens > 0 || clicks > 0 || uniqueOpens > 0 || uniqueClicks > 0;
    }
}
