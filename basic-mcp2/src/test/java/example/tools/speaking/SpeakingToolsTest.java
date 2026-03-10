package example.tools.speaking;

import example.tools.speaking.model.SpeakingEngagement;
import example.tools.speaking.model.SpeakingSearchResult;
import example.tools.speaking.model.SpeakingStats;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

/**
 * Unit tests for SpeakingTools MCP functionality with mocked SpeakingService.
 */
@SpringBootTest
@ActiveProfiles("test")
class SpeakingToolsTest {

    @Autowired
    private SpeakingTools speakingTools;

    @MockitoBean
    private SpeakingService speakingService;

    @Test
    void testGetLatestEngagementsWithDefaultCount() {
        // Given - mock latest engagements
        List<SpeakingEngagement> mockEngagements = List.of(
            new SpeakingEngagement(
                "Spring Boot 3.0 Best Practices",
                "https://springone.com/session1",
                "SpringOne",
                LocalDateTime.of(2024, 12, 15, 10, 0),
                LocalDateTime.of(2024, 12, 15, 11, 0),
                "San Francisco, CA",
                "Deep dive into Spring Boot 3.0 features and best practices for modern application development"
            ),
            new SpeakingEngagement(
                "AI-Powered Applications with Spring AI",
                "https://javazone.com/session2",
                "JavaZone",
                LocalDateTime.now().plusDays(30),
                LocalDateTime.now().plusDays(30).plusHours(1),
                "Oslo, Norway",
                "Building intelligent applications using Spring AI framework"
            )
        );

        when(speakingService.getLatestEngagements(10)).thenReturn(mockEngagements);

        // When
        List<SpeakingEngagement> result = speakingTools.getLatestEngagements(null);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Spring Boot 3.0 Best Practices", result.get(0).title());
        assertEquals("AI-Powered Applications with Spring AI", result.get(1).title());
    }

    @Test
    void testGetLatestEngagementsWithCustomCount() {
        // Given - mock 3 engagements
        List<SpeakingEngagement> mockEngagements = List.of(
            new SpeakingEngagement("Engagement 1", "https://example.com/1", "Event 1",
                LocalDateTime.now().minusDays(1), LocalDateTime.now().minusDays(1).plusHours(1),
                "Location 1", "Description 1"),
            new SpeakingEngagement("Engagement 2", "https://example.com/2", "Event 2",
                LocalDateTime.now().minusDays(2), LocalDateTime.now().minusDays(2).plusHours(1),
                "Location 2", "Description 2"),
            new SpeakingEngagement("Engagement 3", "https://example.com/3", "Event 3",
                LocalDateTime.now().minusDays(3), LocalDateTime.now().minusDays(3).plusHours(1),
                "Location 3", "Description 3")
        );

        when(speakingService.getLatestEngagements(3)).thenReturn(mockEngagements);

        // When
        List<SpeakingEngagement> result = speakingTools.getLatestEngagements("3");

        // Then
        assertNotNull(result);
        assertEquals(3, result.size());
        assertEquals("Engagement 1", result.get(0).title());
    }

    @Test
    void testGetLatestEngagementsEmpty() {
        // Given - no engagements
        when(speakingService.getLatestEngagements(anyInt())).thenReturn(List.of());

        // When
        List<SpeakingEngagement> result = speakingTools.getLatestEngagements("10");

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testGetUpcomingEvents() {
        // Given - mock upcoming engagements
        List<SpeakingEngagement> mockEngagements = List.of(
            new SpeakingEngagement(
                "AI-Powered Applications with Spring AI",
                "https://javazone.com/session2",
                "JavaZone",
                LocalDateTime.now().plusDays(30),
                LocalDateTime.now().plusDays(30).plusHours(1),
                "Oslo, Norway",
                "Building intelligent applications using Spring AI framework"
            )
        );

        when(speakingService.getUpcomingEngagements(5)).thenReturn(mockEngagements);

        // When
        List<SpeakingEngagement> result = speakingTools.getUpcomingEvents("5");

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("AI-Powered Applications with Spring AI", result.get(0).title());
        assertTrue(result.get(0).isUpcoming());
    }

    @Test
    void testGetUpcomingEventsEmpty() {
        // Given - no upcoming engagements
        when(speakingService.getUpcomingEngagements(anyInt())).thenReturn(List.of());

        // When
        List<SpeakingEngagement> result = speakingTools.getUpcomingEvents("10");

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testSearchByTopic() {
        // Given - mock search results
        List<SpeakingEngagement> mockEngagements = List.of(
            new SpeakingEngagement(
                "Spring Boot 3.0 Best Practices",
                "https://springone.com/session1",
                "SpringOne",
                LocalDateTime.of(2024, 12, 15, 10, 0),
                LocalDateTime.of(2024, 12, 15, 11, 0),
                "San Francisco, CA",
                "Deep dive into Spring Boot 3.0 features"
            ),
            new SpeakingEngagement(
                "AI-Powered Applications with Spring AI",
                "https://javazone.com/session2",
                "JavaZone",
                LocalDateTime.now().plusDays(30),
                LocalDateTime.now().plusDays(30).plusHours(1),
                "Oslo, Norway",
                "Building with Spring AI framework"
            )
        );

        SpeakingSearchResult searchResult = SpeakingSearchResult.forKeyword(mockEngagements, "spring");
        when(speakingService.searchEngagementsByKeyword("spring", 10)).thenReturn(searchResult);

        // When
        List<SpeakingEngagement> result = speakingTools.searchByTopic("spring", null);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Spring Boot 3.0 Best Practices", result.get(0).title());
    }

    @Test
    void testSearchByTopicNoResults() {
        // Given - no results
        SpeakingSearchResult emptyResult = SpeakingSearchResult.forKeyword(List.of(), "nonexistent");
        when(speakingService.searchEngagementsByKeyword(anyString(), anyInt())).thenReturn(emptyResult);

        // When
        List<SpeakingEngagement> result = speakingTools.searchByTopic("nonexistent", null);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testSearchByTopicWithNullTopic() {
        // When & Then - should throw exception
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            speakingTools.searchByTopic(null, null);
        });
        assertEquals("Topic parameter is required.", exception.getMessage());
    }

    @Test
    void testSearchByTopicWithEmptyTopic() {
        // When & Then - should throw exception
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            speakingTools.searchByTopic("", null);
        });
        assertEquals("Topic parameter is required.", exception.getMessage());
    }

    @Test
    void testGetSpeakingStats() {
        // Given - mock speaking stats
        SpeakingStats mockStats = new SpeakingStats(
            25,
            5,
            20,
            LocalDateTime.of(2020, 1, 1, 0, 0),
            LocalDateTime.now().plusDays(15),
            "Virtual",
            "Conference",
            Map.of("Virtual", 10, "San Francisco", 8, "New York", 7),
            Map.of("Conference", 20, "Meetup", 3, "Workshop", 2),
            2.1
        );

        when(speakingService.getSpeakingStats()).thenReturn(mockStats);

        // When
        SpeakingStats result = speakingTools.getSpeakingStats();

        // Then
        assertNotNull(result);
        assertEquals(25, result.totalEngagements());
        assertEquals(5, result.upcomingEvents());
        assertEquals(20, result.pastEvents());
        assertEquals("Virtual", result.mostCommonLocation());
        assertEquals("Conference", result.mostCommonEventType());
        assertEquals(2.1, result.averageEventsPerMonth());
    }

    @Test
    void testGetSpeakingStatsEmpty() {
        // Given - empty stats
        SpeakingStats emptyStats = new SpeakingStats(0, 0, 0, null, null, null, null, Map.of(), Map.of(), 0.0);
        when(speakingService.getSpeakingStats()).thenReturn(emptyStats);

        // When
        SpeakingStats result = speakingTools.getSpeakingStats();

        // Then
        assertNotNull(result);
        assertEquals(0, result.totalEngagements());
    }

    @Test
    void testParseCountWithValidNumber() {
        // Given
        when(speakingService.getLatestEngagements(25)).thenReturn(List.of());

        // When
        List<SpeakingEngagement> result = speakingTools.getLatestEngagements("25");

        // Then - parseCount should use 25
        assertNotNull(result);
    }

    @Test
    void testParseCountWithInvalidNumber() {
        // Given
        when(speakingService.getLatestEngagements(10)).thenReturn(List.of());

        // When
        List<SpeakingEngagement> result = speakingTools.getLatestEngagements("invalid");

        // Then - parseCount should use default of 10
        assertNotNull(result);
    }

    @Test
    void testParseCountExceedsMaximum() {
        // Given
        when(speakingService.getLatestEngagements(50)).thenReturn(List.of());

        // When
        List<SpeakingEngagement> result = speakingTools.getLatestEngagements("100");

        // Then - parseCount should cap at 50
        assertNotNull(result);
    }
}
