package example.tools.speaking;

import example.config.SpeakingProperties;
import example.tools.speaking.model.SpeakingEngagement;
import example.tools.speaking.model.SpeakingSearchResult;
import example.tools.speaking.model.SpeakingStats;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Unit tests for Speaking model classes and utilities.
 * These tests verify the behavior of SpeakingEngagement, SpeakingSearchResult, SpeakingStats, and SpeakingProperties.
 */
class SpeakingServiceIntegrationTest {

    @Test
    void testSpeakingPropertiesConfiguration() {
        SpeakingProperties properties = new SpeakingProperties(
            "https://www.danvega.dev/api/speaking",
            Duration.ofMinutes(1)
        );

        assertThat(properties.apiUrl()).isEqualTo("https://www.danvega.dev/api/speaking");
        assertThat(properties.getCacheDurationMinutes()).isEqualTo(1);
        assertThat(properties.isEnabled()).isTrue();
    }

    @Test
    void testSpeakingEngagementBasicFunctionality() {
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime futureDate = now.plusDays(30);
        LocalDateTime pastDate = now.minusDays(30);

        // Test upcoming engagement
        SpeakingEngagement upcomingEngagement = new SpeakingEngagement(
            "Spring AI Workshop",
            "https://springone.io/session1",
            "SpringOne Conference",
            futureDate,
            futureDate.plusHours(2),
            "San Francisco, CA",
            "Hands-on workshop covering Spring AI fundamentals and advanced topics"
        );

        assertThat(upcomingEngagement.isUpcoming()).isTrue();
        assertThat(upcomingEngagement.isPast()).isFalse();
        assertThat(upcomingEngagement.isOngoing()).isFalse();
        assertThat(upcomingEngagement.getEventStatus()).isEqualTo("Upcoming");
        assertThat(upcomingEngagement.getEventType()).isEqualTo("Conference");
        assertThat(upcomingEngagement.hasUrl()).isTrue();
        assertThat(upcomingEngagement.getFormattedDateRange()).isNotEmpty();

        // Test past engagement
        SpeakingEngagement pastEngagement = new SpeakingEngagement(
            "Java Microservices",
            "https://javazone.no/session2",
            "JavaZone",
            pastDate,
            pastDate.plusHours(1),
            "Oslo, Norway",
            "Building microservices with Spring Boot and Cloud"
        );

        assertThat(pastEngagement.isUpcoming()).isFalse();
        assertThat(pastEngagement.isPast()).isTrue();
        assertThat(pastEngagement.getEventStatus()).isEqualTo("Past");

        // Test topic extraction
        String topics = upcomingEngagement.extractTopics();
        assertThat(topics).contains("spring");
        assertThat(topics).contains("ai");
    }

    @Test
    void testSpeakingSearchResultFunctionality() {
        List<SpeakingEngagement> engagements = List.of(
            SpeakingEngagement.basic("Spring Boot Talk", "Conference A", LocalDateTime.now().plusDays(10), "City A"),
            SpeakingEngagement.basic("Java Performance", "Conference B", LocalDateTime.now().minusDays(10), "City B"),
            SpeakingEngagement.basic("Cloud Architecture", "Meetup C", LocalDateTime.now().plusDays(20), "City C")
        );

        SpeakingSearchResult keywordResult = SpeakingSearchResult.forKeyword(engagements, "spring");
        assertThat(keywordResult.hasResults()).isTrue();
        assertThat(keywordResult.totalMatches()).isEqualTo(3);
        assertThat(keywordResult.getSearchSummary()).contains("Found 3 results for keyword search for 'spring'");

        SpeakingSearchResult upcomingResult = SpeakingSearchResult.forUpcoming(
            engagements.stream().filter(SpeakingEngagement::isUpcoming).toList()
        );
        assertThat(upcomingResult.getUpcomingCount()).isEqualTo(2);
        assertThat(upcomingResult.getPastCount()).isEqualTo(0);

        SpeakingSearchResult emptyResult = SpeakingSearchResult.forKeyword(List.of(), "nonexistent");
        assertThat(emptyResult.hasResults()).isFalse();
        assertThat(emptyResult.getSearchSummary()).contains("No results found");
    }

    @Test
    void testSpeakingStatsFunctionality() {
        SpeakingStats stats = new SpeakingStats(
            15,
            4,
            11,
            LocalDateTime.of(2021, 6, 1, 0, 0),
            LocalDateTime.now().plusDays(45),
            "Virtual",
            "Conference",
            java.util.Map.of(
                "Virtual", 8,
                "San Francisco", 4,
                "New York", 2,
                "Austin", 1
            ),
            java.util.Map.of(
                "Conference", 10,
                "Meetup", 3,
                "Workshop", 2
            ),
            1.2
        );

        assertThat(stats.totalEngagements()).isEqualTo(15);
        assertThat(stats.upcomingEvents()).isEqualTo(4);
        assertThat(stats.pastEvents()).isEqualTo(11);
        assertThat(stats.hasUpcomingEvents()).isTrue();
        assertThat(stats.isActiveSpeaker()).isTrue();

        assertThat(stats.getSpeakingTimespan()).contains("year");
        assertThat(stats.getSpeakingFrequency()).isNotEmpty();
        assertThat(stats.getNextEventInfo()).contains("Next event");

        assertThat(stats.getTopLocations()).contains("Virtual (8)");
        assertThat(stats.getTopLocations()).contains("San Francisco (4)");
        assertThat(stats.getTopLocations()).contains("New York (2)");

        assertThat(stats.getTopEventTypes()).contains("Conference (10)");
        assertThat(stats.getTopEventTypes()).contains("Meetup (3)");
        assertThat(stats.getTopEventTypes()).contains("Workshop (2)");

        String distribution = stats.getEventTypeDistribution();
        assertThat(distribution).contains("Conference: 66.7%");
        assertThat(distribution).contains("Meetup: 20.0%");
        assertThat(distribution).contains("Workshop: 13.3%");
    }

    @Test
    void testDateFormatting() {
        LocalDateTime startDate = LocalDateTime.of(2024, 12, 15, 14, 30);
        LocalDateTime endDate = LocalDateTime.of(2024, 12, 15, 16, 0);

        SpeakingEngagement sameDay = new SpeakingEngagement(
            "Same Day Event",
            null,
            "Conference",
            startDate,
            endDate,
            "Location",
            null
        );

        String formattedRange = sameDay.getFormattedDateRange();
        assertThat(formattedRange).contains("Dec 15, 2024");
        assertThat(formattedRange).contains("2:30 PM");
        assertThat(formattedRange).contains("4:00 PM");

        SpeakingEngagement multiDay = new SpeakingEngagement(
            "Multi Day Event",
            null,
            "Conference",
            startDate,
            startDate.plusDays(2),
            "Location",
            null
        );

        String multiDayRange = multiDay.getFormattedDateRange();
        assertThat(multiDayRange).contains("Dec 15, 2024 - Dec 17, 2024");
    }

    @Test
    void testEventTypeClassification() {
        SpeakingEngagement conference = new SpeakingEngagement(
            "Title", null, "Spring Conference 2024", null, null, "Location", null
        );

        SpeakingEngagement meetup = new SpeakingEngagement(
            "Title", null, "Java User Group Meetup", null, null, "Location", null
        );

        SpeakingEngagement workshop = new SpeakingEngagement(
            "Title", null, "Spring Boot Workshop", null, null, "Location", null
        );

        SpeakingEngagement webinar = new SpeakingEngagement(
            "Title", null, "Virtual Webinar Series", null, null, "Location", null
        );

        SpeakingEngagement podcast = new SpeakingEngagement(
            "Title", null, "Tech Podcast Interview", null, null, "Location", null
        );

        assertThat(conference.getEventType()).isEqualTo("Conference");
        assertThat(meetup.getEventType()).isEqualTo("Meetup");
        assertThat(workshop.getEventType()).isEqualTo("Workshop");
        assertThat(webinar.getEventType()).isEqualTo("Webinar");
        assertThat(podcast.getEventType()).isEqualTo("Podcast");
    }

    @Test
    void testPropertiesValidation() {
        // Test minimum cache duration
        SpeakingProperties validProps = new SpeakingProperties(
            "https://api.example.com/speaking",
            Duration.ofMinutes(1)
        );
        assertThat(validProps.getCacheDurationMinutes()).isEqualTo(1);

        // Test default cache duration
        SpeakingProperties defaultProps = new SpeakingProperties(
            "https://api.example.com/speaking",
            null
        );
        assertThat(defaultProps.getCacheDurationMinutes()).isEqualTo(30);
    }

    @Test
    void testSpeakingEngagementDescriptionHandling() {
        String longDescription = "This is a very long description that exceeds the normal display limit. ".repeat(10);

        SpeakingEngagement engagement = new SpeakingEngagement(
            "Title",
            "https://example.com",
            "Event",
            LocalDateTime.now(),
            null,
            "Location",
            longDescription
        );

        String shortDesc = engagement.getShortDescription();
        assertThat(shortDesc).hasSize(303); // 300 chars + "..."
        assertThat(shortDesc).endsWith("...");

        // Test empty description
        SpeakingEngagement emptyDesc = new SpeakingEngagement(
            "Title", null, "Event", null, null, "Location", ""
        );
        assertThat(emptyDesc.getShortDescription()).isEmpty();

        // Test null description
        SpeakingEngagement nullDesc = new SpeakingEngagement(
            "Title", null, "Event", null, null, "Location", null
        );
        assertThat(nullDesc.getShortDescription()).isEmpty();
    }
}