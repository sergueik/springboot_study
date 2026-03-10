package example.tools.speaking.model;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;
import java.util.Map;

/**
 * Statistics and analytics for speaking engagements
 */
public record SpeakingStats(
        int totalEngagements,
        int upcomingEvents,
        int pastEvents,
        LocalDateTime firstEventDate,
        LocalDateTime nextEventDate,
        String mostCommonLocation,
        String mostCommonEventType,
        Map<String, Integer> locationCounts,
        Map<String, Integer> eventTypeCounts,
        double averageEventsPerMonth
) {

    /**
     * Get speaking timespan description
     */
    public String getSpeakingTimespan() {
        if (firstEventDate == null) {
            return "No events recorded";
        }

        LocalDateTime now = LocalDateTime.now();
        long monthsBetween = ChronoUnit.MONTHS.between(firstEventDate, now);

        if (monthsBetween < 1) {
            return "Less than 1 month";
        } else if (monthsBetween < 12) {
            return monthsBetween + " months";
        } else {
            long years = monthsBetween / 12;
            long remainingMonths = monthsBetween % 12;
            if (remainingMonths == 0) {
                return years + " year" + (years > 1 ? "s" : "");
            } else {
                return years + " year" + (years > 1 ? "s" : "") + " and " + remainingMonths + " month" + (remainingMonths > 1 ? "s" : "");
            }
        }
    }

    /**
     * Get speaking frequency description
     */
    public String getSpeakingFrequency() {
        if (averageEventsPerMonth == 0) {
            return "No regular schedule";
        }

        if (averageEventsPerMonth < 0.5) {
            return "Less than monthly";
        } else if (averageEventsPerMonth < 1) {
            return "About monthly";
        } else if (averageEventsPerMonth < 2) {
            return "1-2 times per month";
        } else if (averageEventsPerMonth < 4) {
            return "2-4 times per month";
        } else {
            return "Very active (4+ per month)";
        }
    }

    /**
     * Get next event information
     */
    public String getNextEventInfo() {
        if (nextEventDate == null) {
            return "No upcoming events scheduled";
        }

        long daysUntil = ChronoUnit.DAYS.between(LocalDateTime.now(), nextEventDate);
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("MMM dd, yyyy");

        if (daysUntil < 0) {
            return "Next event is overdue or ongoing";
        } else if (daysUntil == 0) {
            return "Next event is today!";
        } else if (daysUntil == 1) {
            return "Next event is tomorrow (" + nextEventDate.format(formatter) + ")";
        } else if (daysUntil <= 7) {
            return "Next event in " + daysUntil + " days (" + nextEventDate.format(formatter) + ")";
        } else if (daysUntil <= 30) {
            return "Next event in " + daysUntil + " days (" + nextEventDate.format(formatter) + ")";
        } else {
            return "Next event on " + nextEventDate.format(formatter) + " (" + daysUntil + " days away)";
        }
    }

    /**
     * Get top locations (up to 3)
     */
    public String getTopLocations() {
        if (locationCounts == null || locationCounts.isEmpty()) {
            return "No location data available";
        }

        return locationCounts.entrySet().stream()
                .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
                .limit(3)
                .map(entry -> entry.getKey() + " (" + entry.getValue() + ")")
                .reduce((a, b) -> a + ", " + b)
                .orElse("No locations");
    }

    /**
     * Get top event types (up to 3)
     */
    public String getTopEventTypes() {
        if (eventTypeCounts == null || eventTypeCounts.isEmpty()) {
            return "No event type data available";
        }

        return eventTypeCounts.entrySet().stream()
                .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
                .limit(3)
                .map(entry -> entry.getKey() + " (" + entry.getValue() + ")")
                .reduce((a, b) -> a + ", " + b)
                .orElse("No event types");
    }

    /**
     * Get current year speaking activity
     */
    public int getCurrentYearEvents() {
        // This would need to be calculated from the actual events list
        // For now, we'll return a placeholder that should be set by the service
        return 0;
    }

    /**
     * Get speaking engagement percentage by type
     */
    public String getEventTypeDistribution() {
        if (eventTypeCounts == null || eventTypeCounts.isEmpty() || totalEngagements == 0) {
            return "No event type distribution available";
        }

        StringBuilder distribution = new StringBuilder();
        eventTypeCounts.entrySet().stream()
                .sorted(Map.Entry.<String, Integer>comparingByValue().reversed())
                .forEach(entry -> {
                    double percentage = (entry.getValue() * 100.0) / totalEngagements;
                    distribution.append(String.format("%s: %.1f%%, ", entry.getKey(), percentage));
                });

        String result = distribution.toString();
        return result.endsWith(", ") ? result.substring(0, result.length() - 2) : result;
    }

    /**
     * Check if there are any upcoming events
     */
    public boolean hasUpcomingEvents() {
        return upcomingEvents > 0;
    }

    /**
     * Check if speaker is currently active (has recent or upcoming events)
     */
    public boolean isActiveSpeaker() {
        return upcomingEvents > 0 || (firstEventDate != null &&
               ChronoUnit.MONTHS.between(firstEventDate, LocalDateTime.now()) <= 6);
    }
}