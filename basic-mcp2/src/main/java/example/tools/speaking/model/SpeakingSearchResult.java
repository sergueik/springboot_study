package example.tools.speaking.model;

import java.util.List;

/**
 * Search results wrapper for speaking engagement queries
 */
public record SpeakingSearchResult(
        List<SpeakingEngagement> engagements,
        int totalMatches,
        String searchCriteria,
        String resultDescription
) {

    /**
     * Create search result for keyword search
     */
    public static SpeakingSearchResult forKeyword(List<SpeakingEngagement> engagements, String keyword) {
        return new SpeakingSearchResult(
                engagements,
                engagements.size(),
                keyword,
                "keyword search for '" + keyword + "'"
        );
    }

    /**
     * Create search result for date range search
     */
    public static SpeakingSearchResult forDateRange(List<SpeakingEngagement> engagements, String dateRange) {
        return new SpeakingSearchResult(
                engagements,
                engagements.size(),
                dateRange,
                "date range " + dateRange
        );
    }

    /**
     * Create search result for location search
     */
    public static SpeakingSearchResult forLocation(List<SpeakingEngagement> engagements, String location) {
        return new SpeakingSearchResult(
                engagements,
                engagements.size(),
                location,
                "location filter for '" + location + "'"
        );
    }

    /**
     * Create search result for event type search
     */
    public static SpeakingSearchResult forEventType(List<SpeakingEngagement> engagements, String eventType) {
        return new SpeakingSearchResult(
                engagements,
                engagements.size(),
                eventType,
                "event type filter for '" + eventType + "'"
        );
    }

    /**
     * Create search result for upcoming events
     */
    public static SpeakingSearchResult forUpcoming(List<SpeakingEngagement> engagements) {
        return new SpeakingSearchResult(
                engagements,
                engagements.size(),
                "upcoming",
                "upcoming events"
        );
    }

    /**
     * Create search result for past events
     */
    public static SpeakingSearchResult forPast(List<SpeakingEngagement> engagements) {
        return new SpeakingSearchResult(
                engagements,
                engagements.size(),
                "past",
                "past events"
        );
    }

    /**
     * Check if search returned any results
     */
    public boolean hasResults() {
        return engagements != null && !engagements.isEmpty();
    }

    /**
     * Get search result summary
     */
    public String getSearchSummary() {
        if (!hasResults()) {
            return "No results found for " + resultDescription;
        }

        return String.format("Found %d result%s for %s",
                totalMatches,
                totalMatches == 1 ? "" : "s",
                resultDescription);
    }

    /**
     * Get the count of upcoming events in results
     */
    public long getUpcomingCount() {
        return engagements.stream()
                .filter(SpeakingEngagement::isUpcoming)
                .count();
    }

    /**
     * Get the count of past events in results
     */
    public long getPastCount() {
        return engagements.stream()
                .filter(SpeakingEngagement::isPast)
                .count();
    }

    /**
     * Get the count of ongoing events in results
     */
    public long getOngoingCount() {
        return engagements.stream()
                .filter(SpeakingEngagement::isOngoing)
                .count();
    }

    /**
     * Get status breakdown of results
     */
    public String getStatusBreakdown() {
        if (!hasResults()) {
            return "No events to analyze";
        }

        long upcoming = getUpcomingCount();
        long past = getPastCount();
        long ongoing = getOngoingCount();

        StringBuilder breakdown = new StringBuilder();
        if (upcoming > 0) {
            breakdown.append(upcoming).append(" upcoming");
        }
        if (ongoing > 0) {
            if (breakdown.length() > 0) breakdown.append(", ");
            breakdown.append(ongoing).append(" ongoing");
        }
        if (past > 0) {
            if (breakdown.length() > 0) breakdown.append(", ");
            breakdown.append(past).append(" past");
        }

        return breakdown.length() > 0 ? breakdown.toString() : "No status information";
    }
}