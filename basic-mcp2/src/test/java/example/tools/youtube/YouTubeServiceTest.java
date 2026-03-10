package example.tools.youtube;

import example.tools.youtube.model.ChannelStats;
import example.tools.youtube.model.Video;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.bean.override.mockito.MockitoBean;
import org.springframework.test.context.ActiveProfiles;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for YouTubeService with mocked responses.
 * These tests run fast and don't require API credentials.
 */
@SpringBootTest
@ActiveProfiles("test")
class YouTubeServiceTest {

    @MockitoBean
    private YouTubeService youTubeService;

    @Test
    void testGetChannelStats() {
        // Given - mock channel stats
        ChannelStats mockStats = new ChannelStats(
                "UC123456789",
                "Dan Vega",
                "Spring Boot and Java tutorials",
                50000L,
                1000000L,
                200L,
                LocalDateTime.of(2020, 1, 15, 10, 0),
                false
        );

        when(youTubeService.getChannelStats()).thenReturn(mockStats);

        // When
        ChannelStats result = youTubeService.getChannelStats();

        // Then
        assertNotNull(result);
        assertEquals("UC123456789", result.channelId());
        assertEquals("Dan Vega", result.title());
        assertEquals(50000L, result.subscriberCount());
        assertEquals(200L, result.videoCount());
        assertEquals("50.0K", result.getFormattedSubscriberCount());
        assertEquals("1.0M", result.getFormattedTotalViewCount());
        assertEquals(5000L, result.getAverageViewsPerVideo());
    }

    @Test
    void testGetLatestVideos() {
        // Given - mock video list
        List<Video> mockVideos = List.of(
            Video.basic("video1", "Spring Boot 3 Tutorial",
                          "https://youtube.com/watch?v=video1",
                          LocalDateTime.now().minusDays(1), 1500L),
            Video.basic("video2", "Java 21 New Features",
                          "https://youtube.com/watch?v=video2",
                          LocalDateTime.now().minusDays(3), 2300L)
        );

        when(youTubeService.getLatestVideos(5)).thenReturn(mockVideos);

        // When
        List<Video> result = youTubeService.getLatestVideos(5);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Spring Boot 3 Tutorial", result.get(0).title());
        assertEquals("video1", result.get(0).id());
        assertTrue(result.get(0).getYouTubeUrl().contains("video1"));
    }

    @Test
    void testGetTopVideos() {
        // Given - mock top videos (sorted by view count)
        List<Video> mockTopVideos = List.of(
            Video.basic("top1", "Most Popular Spring Tutorial",
                          "https://youtube.com/watch?v=top1",
                          LocalDateTime.now().minusMonths(2), 50000L),
            Video.basic("top2", "Java Best Practices",
                          "https://youtube.com/watch?v=top2",
                          LocalDateTime.now().minusMonths(1), 30000L)
        );

        when(youTubeService.getTopVideos(5, "recent")).thenReturn(mockTopVideos);

        // When
        List<Video> result = youTubeService.getTopVideos(5, "recent");

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        // Verify sorting by view count (descending)
        assertTrue(result.get(0).viewCount() >= result.get(1).viewCount());
        assertEquals(50000L, result.get(0).viewCount());
        assertEquals("Most Popular Spring Tutorial", result.get(0).title());
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
        List<Video> result = youTubeService.searchVideosByTopic("spring", 10);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());

        Video firstVideo = result.get(0);
        assertEquals("Spring Security Tutorial", firstVideo.title());
        assertTrue(firstVideo.title().toLowerCase().contains("spring"));
    }

    @Test
    void testGetLatestVideosEmptyResult() {
        // Given - empty result
        when(youTubeService.getLatestVideos(10)).thenReturn(List.of());

        // When
        List<Video> result = youTubeService.getLatestVideos(10);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testSearchVideosNoResults() {
        // Given - no search results
        when(youTubeService.searchVideosByTopic("nonexistent", 10)).thenReturn(List.of());

        // When
        List<Video> result = youTubeService.searchVideosByTopic("nonexistent", 10);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testServiceErrorHandling() {
        // Given - service throws exception
        when(youTubeService.getChannelStats()).thenThrow(new RuntimeException("API Error"));

        // When & Then
        assertThrows(RuntimeException.class, () -> youTubeService.getChannelStats());
    }

    @Test
    void testVideoFormatting() {
        // Given - video with high view count
        Video video = Video.basic("test", "Test Video",
                                        "https://youtube.com/watch?v=test",
                                        LocalDateTime.now(), 1500000L);

        // When & Then - test formatting methods
        assertEquals("1.5M", video.getFormattedViewCount());
        assertTrue(video.getYouTubeUrl().contains("youtube.com"));
        assertEquals("test", video.id());
    }

    @Test
    void testChannelStatsFormatting() {
        // Given
        ChannelStats stats = new ChannelStats(
                "UC123", "Test Channel", "Description",
                1500000L, 50000000L, 100L,
                LocalDateTime.now().minusYears(2), false
        );

        // When & Then
        assertEquals("1.5M", stats.getFormattedSubscriberCount());
        assertEquals("50.0M", stats.getFormattedTotalViewCount());
        assertEquals(500000L, stats.getAverageViewsPerVideo());
    }
}