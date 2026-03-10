package example.tools.speaking.model;

import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;

/**
 * Represents a speaking engagement for MCP tool responses
 */
public record SpeakingEngagement(
        String title,
        String url,
        String name,
        LocalDateTime startDate,
        LocalDateTime endDate,
        String location,
        String description
) {

    /**
     * Create a basic SpeakingEngagement with essential information
     */
    public static SpeakingEngagement basic(String title, String name, LocalDateTime startDate, String location) {
        return new SpeakingEngagement(title, null, name, startDate, null, location, null);
    }

    /**
     * Get the full event URL (ensure it's absolute)
     */
    public String getFullUrl() {
        if (url != null && url.startsWith("http")) {
            return url;
        }
        return url != null ? url : "";
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
     * Check if this is an upcoming event
     */
    public boolean isUpcoming() {
        LocalDateTime now = LocalDateTime.now();
        return startDate != null && startDate.isAfter(now);
    }

    /**
     * Check if this is a past event
     */
    public boolean isPast() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime eventEnd = endDate != null ? endDate : startDate;
        return eventEnd != null && eventEnd.isBefore(now);
    }

    /**
     * Check if this event is currently ongoing
     */
    public boolean isOngoing() {
        LocalDateTime now = LocalDateTime.now();
        if (startDate == null) return false;

        LocalDateTime eventEnd = endDate != null ? endDate : startDate.plusHours(2); // Default 2-hour duration
        return !startDate.isAfter(now) && !eventEnd.isBefore(now);
    }

    /**
     * Get formatted date range for display
     */
    public String getFormattedDateRange() {
        if (startDate == null) {
            return "Date TBD";
        }

        DateTimeFormatter dateFormatter = DateTimeFormatter.ofPattern("MMM dd, yyyy");
        DateTimeFormatter timeFormatter = DateTimeFormatter.ofPattern("h:mm a");

        if (endDate == null) {
            // Single date/time
            return startDate.format(dateFormatter) + " at " + startDate.format(timeFormatter);
        }

        if (startDate.toLocalDate().equals(endDate.toLocalDate())) {
            // Same day, different times
            return startDate.format(dateFormatter) + " from " +
                   startDate.format(timeFormatter) + " to " + endDate.format(timeFormatter);
        } else {
            // Multi-day event
            return startDate.format(dateFormatter) + " - " + endDate.format(dateFormatter);
        }
    }

    /**
     * Get event status (upcoming, ongoing, past)
     */
    public String getEventStatus() {
        if (isUpcoming()) {
            return "Upcoming";
        } else if (isOngoing()) {
            return "Ongoing";
        } else {
            return "Past";
        }
    }

    /**
     * Check if event has a valid URL
     */
    public boolean hasUrl() {
        return url != null && !url.trim().isEmpty();
    }

    /**
     * Get event type based on patterns in the name
     */
    public String getEventType() {
        if (name == null) {
            return "Speaking Event";
        }

        String lowerName = name.toLowerCase();
        if (lowerName.contains("conference")) {
            return "Conference";
        } else if (lowerName.contains("meetup") || lowerName.contains("user group")) {
            return "Meetup";
        } else if (lowerName.contains("workshop")) {
            return "Workshop";
        } else if (lowerName.contains("webinar") || lowerName.contains("virtual")) {
            return "Webinar";
        } else if (lowerName.contains("podcast")) {
            return "Podcast";
        } else {
            return "Speaking Event";
        }
    }

    /**
     * Extract potential topics from title and description
     */
    public String extractTopics() {
        String content = (title + " " + (description != null ? description : "")).toLowerCase();

        return java.util.List.of("spring", "java", "ai", "boot", "microservices", "cloud", "kubernetes",
                                 "docker", "rest", "api", "testing", "devops", "aws", "azure", "graphql")
                .stream()
                .filter(topic -> content.contains(topic))
                .reduce((a, b) -> a + ", " + b)
                .orElse("");
    }
}