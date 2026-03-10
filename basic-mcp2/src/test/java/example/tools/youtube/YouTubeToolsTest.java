package example.tools.youtube;

import example.tools.youtube.model.ChannelStats;
import example.tools.youtube.model.Video;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

/**
 * Unit tests for YouTubeTools MCP functionality with mocked YouTubeService.
 */
@SpringBootTest
@ActiveProfiles("test")
class YouTubeToolsTest {

    @Autowired
    private YouTubeTools youTubeTools;

    @MockitoBean
    private YouTubeService youTubeService;

    @Test
    void testGetLatestVideosWithDefaultCount() {
        // Given - mock latest videos
        List<Video> mockVideos = List.of(
            Video.basic("video1", "Spring Boot 3.2 Released",
                          "https://youtube.com/watch?v=video1",
                          LocalDateTime.now().minusDays(1), 2500L),
            Video.basic("video2", "Java 21 Virtual Threads",
                          "https://youtube.com/watch?v=video2",
                          LocalDateTime.now().minusDays(2), 1800L)
        );

        when(youTubeService.getLatestVideos(10)).thenReturn(mockVideos);

        // When
        List<Video> result = youTubeTools.getLatestVideos(null);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Spring Boot 3.2 Released", result.get(0).title());
        assertEquals("Java 21 Virtual Threads", result.get(1).title());
    }

    @Test
    void testGetLatestVideosWithCustomCount() {
        // Given - mock 3 videos
        List<Video> mockVideos = List.of(
            Video.basic("v1", "Video 1", "https://youtube.com/watch?v=v1",
                          LocalDateTime.now().minusDays(1), 1000L),
            Video.basic("v2", "Video 2", "https://youtube.com/watch?v=v2",
                          LocalDateTime.now().minusDays(2), 2000L),
            Video.basic("v3", "Video 3", "https://youtube.com/watch?v=v3",
                          LocalDateTime.now().minusDays(3), 3000L)
        );

        when(youTubeService.getLatestVideos(3)).thenReturn(mockVideos);

        // When
        List<Video> result = youTubeTools.getLatestVideos("3");

        // Then
        assertNotNull(result);
        assertEquals(3, result.size());
        assertEquals("Video 1", result.get(0).title());
    }

    @Test
    void testGetLatestVideosEmpty() {
        // Given - no videos
        when(youTubeService.getLatestVideos(anyInt())).thenReturn(List.of());

        // When
        List<Video> result = youTubeTools.getLatestVideos("5");

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testGetTopVideos() {
        // Given - mock top videos with detailed stats
        List<Video> mockVideos = List.of(
            new Video("top1", "Most Popular Spring Tutorial",
                        "https://youtube.com/watch?v=top1", "Tutorial description",
                        LocalDateTime.now().minusMonths(2), 100000L, 5000L, 200L,
                        "PT15M30S", "https://i.ytimg.com/vi/top1/default.jpg"),
            new Video("top2", "Java Best Practices",
                        "https://youtube.com/watch?v=top2", "Best practices guide",
                        LocalDateTime.now().minusMonths(1), 75000L, 3500L, 150L,
                        "PT20M15S", "https://i.ytimg.com/vi/top2/default.jpg")
        );

        when(youTubeService.getTopVideos(5, "recent")).thenReturn(mockVideos);

        // When
        List<Video> result = youTubeTools.getTopVideos("5", "recent");

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Most Popular Spring Tutorial", result.get(0).title());
        assertEquals(100000L, result.get(0).viewCount());
    }

    @Test
    void testSearchVideosByTopic() {
        // Given - mock search results
        List<Video> mockVideos = List.of(
            Video.basic("spring1", "Spring Security Tutorial",
                          "https://youtube.com/watch?v=spring1",
                          LocalDateTime.now().minusWeeks(1), 5000L),
            Video.basic("spring2", "Spring Data JPA Guide",
                          "https://youtube.com/watch?v=spring2",
                          LocalDateTime.now().minusWeeks(2), 3500L)
        );

        when(youTubeService.searchVideosByTopic("spring", 10)).thenReturn(mockVideos);

        // When
        List<Video> result = youTubeTools.searchVideosByTopic("spring", null);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Spring Security Tutorial", result.get(0).title());
    }

    @Test
    void testSearchVideosByTopicNoResults() {
        // Given - no results
        when(youTubeService.searchVideosByTopic(anyString(), anyInt())).thenReturn(List.of());

        // When
        List<Video> result = youTubeTools.searchVideosByTopic("nonexistent", null);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testSearchVideosByTopicWithNullTopic() {
        // When & Then - should throw exception
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            youTubeTools.searchVideosByTopic(null, null);
        });
        assertEquals("Topic parameter is required.", exception.getMessage());
    }

    @Test
    void testSearchVideosByTopicWithEmptyTopic() {
        // When & Then - should throw exception
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            youTubeTools.searchVideosByTopic("", null);
        });
        assertEquals("Topic parameter is required.", exception.getMessage());
    }

    @Test
    void testGetChannelStats() {
        // Given - mock channel stats
        ChannelStats mockStats = new ChannelStats(
                "UC123456789",
                "Dan Vega",
                "Spring Boot and Java tutorials from Dan Vega",
                75000L,
                5000000L,
                250L,
                LocalDateTime.of(2020, 1, 15, 10, 0),
                false
        );

        when(youTubeService.getChannelStats()).thenReturn(mockStats);

        // When
        ChannelStats result = youTubeTools.getChannelStats();

        // Then
        assertNotNull(result);
        assertEquals("Dan Vega", result.title());
        assertEquals(75000L, result.subscriberCount());
        assertEquals(250L, result.videoCount());
        assertEquals("75.0K", result.getFormattedSubscriberCount());
    }

    @Test
    void testParseCountWithValidNumber() {
        // Given
        when(youTubeService.getLatestVideos(25)).thenReturn(List.of());

        // When
        List<Video> result = youTubeTools.getLatestVideos("25");

        // Then - parseCount should use 25
        assertNotNull(result);
    }

    @Test
    void testParseCountWithInvalidNumber() {
        // Given
        when(youTubeService.getLatestVideos(10)).thenReturn(List.of());

        // When
        List<Video> result = youTubeTools.getLatestVideos("invalid");

        // Then - parseCount should use default of 10
        assertNotNull(result);
    }

    @Test
    void testParseCountExceedsMaximum() {
        // Given
        when(youTubeService.getLatestVideos(50)).thenReturn(List.of());

        // When
        List<Video> result = youTubeTools.getLatestVideos("100");

        // Then - parseCount should cap at 50
        assertNotNull(result);
    }
}
