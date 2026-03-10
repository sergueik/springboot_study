package example.tools.speaking;

import example.config.SpeakingProperties;
import example.tools.speaking.model.SpeakingEngagement;
import example.tools.speaking.model.SpeakingSearchResult;
import example.tools.speaking.model.SpeakingStats;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;
import org.mockito.quality.Strictness;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.mockito.Mockito.when;

@ExtendWith(MockitoExtension.class)
class SpeakingServiceTest {

    @Mock
    private SpeakingProperties speakingProperties;

    private SpeakingService speakingService;

    private List<SpeakingEngagement> testEngagements;

    @BeforeEach
    void setUp() {
        // Create test engagements
        testEngagements = List.of(
            new SpeakingEngagement(
                "Spring Boot 3.0 Best Practices",
                "https://springone.com/session1",
                "SpringOne",
                LocalDateTime.of(2024, 12, 15, 10, 0),
                LocalDateTime.of(2024, 12, 15, 11, 0),
                "San Francisco, CA",
                "Deep dive into Spring Boot 3.0 features and best practices"
            ),
            new SpeakingEngagement(
                "AI-Powered Applications with Spring AI",
                "https://javazone.com/session2",
                "JavaZone",
                LocalDateTime.of(2024, 11, 20, 14, 0),
                LocalDateTime.of(2024, 11, 20, 15, 0),
                "Oslo, Norway",
                "Building intelligent applications using Spring AI framework"
            ),
            new SpeakingEngagement(
                "Microservices with Spring Cloud",
                "https://devnexus.com/session3",
                "DevNexus",
                LocalDateTime.of(2023, 4, 12, 16, 0),
                LocalDateTime.of(2023, 4, 12, 17, 0),
                "Atlanta, GA",
                "Implementing microservices architecture with Spring Cloud"
            )
        );
    }

    @Test
    void testBasicConfiguration() {
        // Test basic configuration setup without mocking
        SpeakingProperties testProps = new SpeakingProperties(
            "https://api.example.com/speaking",
            Duration.ofMinutes(30)
        );

        assertThat(testProps.apiUrl()).isEqualTo("https://api.example.com/speaking");
        assertThat(testProps.getCacheDurationMinutes()).isEqualTo(30L);
    }

    @Test
    void testSpeakingEngagementModel() {
        SpeakingEngagement engagement = testEngagements.get(0);

        assertThat(engagement.title()).isEqualTo("Spring Boot 3.0 Best Practices");
        assertThat(engagement.name()).isEqualTo("SpringOne");
        assertThat(engagement.location()).isEqualTo("San Francisco, CA");
        assertThat(engagement.isUpcoming()).isFalse(); // Past event
        assertThat(engagement.getEventType()).isEqualTo("Speaking Event"); // "SpringOne" doesn't contain "conference"
        assertThat(engagement.hasUrl()).isTrue();
        assertThat(engagement.getFormattedDateRange()).contains("Dec 15, 2024");
    }

    @Test
    void testSpeakingEngagementUpcoming() {
        LocalDateTime futureDate = LocalDateTime.now().plusDays(30);
        SpeakingEngagement futureEngagement = new SpeakingEngagement(
            "Future Talk",
            "https://example.com",
            "Future Conference",
            futureDate,
            futureDate.plusHours(1),
            "Virtual",
            "A talk in the future"
        );

        assertThat(futureEngagement.isUpcoming()).isTrue();
        assertThat(futureEngagement.isPast()).isFalse();
        assertThat(futureEngagement.getEventStatus()).isEqualTo("Upcoming");
    }

    @Test
    void testSpeakingEngagementPast() {
        LocalDateTime pastDate = LocalDateTime.now().minusDays(30);
        SpeakingEngagement pastEngagement = new SpeakingEngagement(
            "Past Talk",
            "https://example.com",
            "Past Conference",
            pastDate,
            pastDate.plusHours(1),
            "Virtual",
            "A talk in the past"
        );

        assertThat(pastEngagement.isUpcoming()).isFalse();
        assertThat(pastEngagement.isPast()).isTrue();
        assertThat(pastEngagement.getEventStatus()).isEqualTo("Past");
    }

    @Test
    void testSpeakingSearchResult() {
        SpeakingSearchResult result = SpeakingSearchResult.forKeyword(testEngagements, "spring");

        assertThat(result.hasResults()).isTrue();
        assertThat(result.totalMatches()).isEqualTo(3);
        assertThat(result.searchCriteria()).isEqualTo("spring");
        assertThat(result.getSearchSummary()).contains("Found 3 results");
    }

    @Test
    void testSpeakingSearchResultEmpty() {
        SpeakingSearchResult result = SpeakingSearchResult.forKeyword(List.of(), "nonexistent");

        assertThat(result.hasResults()).isFalse();
        assertThat(result.totalMatches()).isEqualTo(0);
        assertThat(result.getSearchSummary()).contains("No results found");
    }

    @Test
    void testSpeakingStats() {
        SpeakingStats stats = new SpeakingStats(
            10,
            3,
            7,
            LocalDateTime.of(2020, 1, 1, 0, 0),
            LocalDateTime.now().plusDays(30),
            "Virtual",
            "Conference",
            Map.of("Virtual", 5, "San Francisco", 3, "Atlanta", 2),
            Map.of("Conference", 8, "Meetup", 2),
            2.5
        );

        assertThat(stats.totalEngagements()).isEqualTo(10);
        assertThat(stats.upcomingEvents()).isEqualTo(3);
        assertThat(stats.pastEvents()).isEqualTo(7);
        assertThat(stats.hasUpcomingEvents()).isTrue();
        assertThat(stats.isActiveSpeaker()).isTrue();
        assertThat(stats.getTopLocations()).contains("Virtual (5)");
        assertThat(stats.getTopEventTypes()).contains("Conference (8)");
    }

    @Test
    void testSpeakingPropertiesValidation() {
        // Test valid properties
        SpeakingProperties validProps = new SpeakingProperties(
            "https://api.example.com/speaking",
            Duration.ofMinutes(30)
        );

        assertThat(validProps.isEnabled()).isTrue();
        assertThat(validProps.getCacheDurationMinutes()).isEqualTo(30);
    }

    @Test
    void testEventTypeExtraction() {
        SpeakingEngagement conferenceEvent = new SpeakingEngagement(
            "Test Talk", null, "SpringOne Conference", null, null, "Virtual", null
        );

        SpeakingEngagement meetupEvent = new SpeakingEngagement(
            "Test Talk", null, "Java User Group Meetup", null, null, "Virtual", null
        );

        SpeakingEngagement workshopEvent = new SpeakingEngagement(
            "Test Talk", null, "Spring Workshop", null, null, "Virtual", null
        );

        assertThat(conferenceEvent.getEventType()).isEqualTo("Conference");
        assertThat(meetupEvent.getEventType()).isEqualTo("Meetup");
        assertThat(workshopEvent.getEventType()).isEqualTo("Workshop");
    }

    @Test
    void testTopicExtraction() {
        SpeakingEngagement engagement = new SpeakingEngagement(
            "Spring Boot and Java Microservices",
            null,
            "Conference",
            null,
            null,
            "Virtual",
            "Learn about Spring Boot microservices with Java and cloud deployment"
        );

        String topics = engagement.extractTopics();
        assertThat(topics).contains("spring");
        assertThat(topics).contains("java");
        assertThat(topics).contains("microservices");
        assertThat(topics).contains("cloud");
    }
}