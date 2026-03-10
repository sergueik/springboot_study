package example.tools.podcast;

import example.tools.podcast.model.Episode;
import example.tools.podcast.model.PodcastStats;
import example.tools.podcast.model.Show;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.Mock;
import org.mockito.junit.jupiter.MockitoExtension;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.*;
import static org.mockito.Mockito.*;

/**
 * Unit tests for PodcastTools MCP class
 */
@ExtendWith(MockitoExtension.class)
class PodcastToolsTest {

    @Mock
    private PodcastService podcastService;

    private PodcastTools podcastTools;

    @BeforeEach
    void setUp() {
        podcastTools = new PodcastTools(podcastService);
    }

    @Test
    void getShows_ShouldReturnListOfShows() {
        // Arrange
        List<Show> mockShows = List.of(
            new Show("1", "Spring Office Hours", "A podcast about Spring",
                    "Dan Vega", "https://example.com", null, "published", LocalDateTime.now()),
            new Show("2", "Fundamentals of Software Engineering", "A podcast about software engineering",
                    "Dan Vega", "https://example.com", null, "published", LocalDateTime.now())
        );

        when(podcastService.getAllShows()).thenReturn(mockShows);

        // Act
        List<Show> result = podcastTools.getShows();

        // Assert
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Spring Office Hours", result.get(0).title());
        assertEquals("Fundamentals of Software Engineering", result.get(1).title());
        verify(podcastService).getAllShows();
    }

    @Test
    void getLatestEpisodes_WithDefaultCount_ShouldReturnListOfEpisodes() {
        // Arrange
        List<Episode> mockEpisodes = List.of(
            new Episode("1", "Episode 100", "Discussion about Spring AI", "show1", "Spring Office Hours",
                    LocalDateTime.of(2024, 1, 15, 10, 0),
                    "https://example.com/audio1.mp3", "3600 seconds", "published", null, 100),
            new Episode("2", "Episode 50", "Understanding Java Streams", "show2", "Fundamentals of Software Engineering",
                    LocalDateTime.of(2024, 1, 10, 14, 30),
                    "https://example.com/audio2.mp3", "2700 seconds", "published", null, 50)
        );

        when(podcastService.getLatestEpisodes(10, null)).thenReturn(mockEpisodes);

        // Act
        List<Episode> result = podcastTools.getLatestEpisodes(null, null);

        // Assert
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Episode 100", result.get(0).title());
        assertEquals("Episode 50", result.get(1).title());
        verify(podcastService).getLatestEpisodes(10, null);
    }

    @Test
    void getLatestEpisodes_WithCustomCount_ShouldRespectCount() {
        // Arrange
        when(podcastService.getLatestEpisodes(5, null)).thenReturn(List.of());

        // Act
        List<Episode> result = podcastTools.getLatestEpisodes("5", null);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(podcastService).getLatestEpisodes(5, null);
    }

    @Test
    void getLatestEpisodes_WithShowFilter_ShouldPassShowToService() {
        // Arrange
        List<Episode> mockEpisodes = List.of(
            new Episode("1", "Episode 100", "Discussion", "show1", "Spring Office Hours",
                    LocalDateTime.now(), null, null, "published", null, 100)
        );

        when(podcastService.getLatestEpisodes(10, "Spring Office Hours")).thenReturn(mockEpisodes);

        // Act
        List<Episode> result = podcastTools.getLatestEpisodes(null, "Spring Office Hours");

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("Spring Office Hours", result.get(0).showTitle());
        verify(podcastService).getLatestEpisodes(10, "Spring Office Hours");
    }

    @Test
    void getLatestEpisodes_WithInvalidCount_ShouldUseDefault() {
        // Arrange
        when(podcastService.getLatestEpisodes(10, null)).thenReturn(List.of());

        // Act
        List<Episode> result = podcastTools.getLatestEpisodes("invalid", null);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(podcastService).getLatestEpisodes(10, null);
    }

    @Test
    void searchEpisodes_WithValidKeyword_ShouldReturnListOfEpisodes() {
        // Arrange
        List<Episode> mockEpisodes = List.of(
            new Episode("1", "Spring Security Deep Dive", "Complete guide to Spring Security",
                    "show1", "Spring Office Hours",
                    LocalDateTime.of(2024, 1, 20, 9, 0),
                    null, null, "published", null, 95)
        );

        when(podcastService.searchEpisodes("spring", 10, null)).thenReturn(mockEpisodes);

        // Act
        List<Episode> result = podcastTools.searchEpisodes("spring", null, null);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("Spring Security Deep Dive", result.get(0).title());
        verify(podcastService).searchEpisodes("spring", 10, null);
    }

    @Test
    void searchEpisodes_WithNullKeyword_ShouldThrowException() {
        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            podcastTools.searchEpisodes(null, null, null);
        });
        assertEquals("Keyword parameter is required.", exception.getMessage());
        verifyNoInteractions(podcastService);
    }

    @Test
    void searchEpisodes_WithEmptyKeyword_ShouldThrowException() {
        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            podcastTools.searchEpisodes("", null, null);
        });
        assertEquals("Keyword parameter is required.", exception.getMessage());
        verifyNoInteractions(podcastService);
    }

    @Test
    void searchEpisodes_WithShowFilter_ShouldPassShowToService() {
        // Arrange
        List<Episode> mockEpisodes = List.of(
            new Episode("1", "Java Fundamentals", "Introduction to Java",
                    "show2", "Fundamentals of Software Engineering",
                    LocalDateTime.now(), null, null, "published", null, 10)
        );

        when(podcastService.searchEpisodes("java", 10, "Fundamentals")).thenReturn(mockEpisodes);

        // Act
        List<Episode> result = podcastTools.searchEpisodes("java", null, "Fundamentals");

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("Java Fundamentals", result.get(0).title());
        verify(podcastService).searchEpisodes("java", 10, "Fundamentals");
    }

    @Test
    void searchEpisodes_WithNoResults_ShouldReturnEmptyList() {
        // Arrange
        when(podcastService.searchEpisodes("nonexistent", 10, null)).thenReturn(List.of());

        // Act
        List<Episode> result = podcastTools.searchEpisodes("nonexistent", null, null);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(podcastService).searchEpisodes("nonexistent", 10, null);
    }

    @Test
    void getEpisodeDetails_WithValidId_ShouldReturnEpisode() {
        // Arrange
        Episode mockEpisode = new Episode(
                "123", "Episode 100: Spring AI Deep Dive",
                "A comprehensive discussion about Spring AI features and best practices",
                "show1", "Spring Office Hours",
                LocalDateTime.of(2024, 1, 15, 10, 0),
                "https://example.com/audio.mp3", "3600 seconds",
                "published", 2, 100
        );

        when(podcastService.getEpisodeById("123")).thenReturn(mockEpisode);

        // Act
        Episode result = podcastTools.getEpisodeDetails("123");

        // Assert
        assertNotNull(result);
        assertEquals("123", result.id());
        assertEquals("Episode 100: Spring AI Deep Dive", result.title());
        assertEquals("Spring Office Hours", result.showTitle());
        assertEquals(Integer.valueOf(2), result.season());
        assertEquals(Integer.valueOf(100), result.number());
        verify(podcastService).getEpisodeById("123");
    }

    @Test
    void getEpisodeDetails_WithNullId_ShouldThrowException() {
        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            podcastTools.getEpisodeDetails(null);
        });
        assertEquals("Episode ID parameter is required.", exception.getMessage());
        verifyNoInteractions(podcastService);
    }

    @Test
    void getEpisodeDetails_WithEmptyId_ShouldThrowException() {
        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            podcastTools.getEpisodeDetails("   ");
        });
        assertEquals("Episode ID parameter is required.", exception.getMessage());
        verifyNoInteractions(podcastService);
    }

    @Test
    void getPodcastStats_ShouldReturnStatsObject() {
        // Arrange
        List<PodcastStats.ShowSummary> summaries = List.of(
                new PodcastStats.ShowSummary("Spring Office Hours", 100, LocalDateTime.of(2024, 1, 15, 0, 0)),
                new PodcastStats.ShowSummary("Fundamentals of Software Engineering", 50, LocalDateTime.of(2024, 1, 10, 0, 0))
        );

        PodcastStats mockStats = new PodcastStats(
                2,
                150,
                LocalDateTime.of(2024, 1, 15, 0, 0),
                "Episode 100: Spring AI Deep Dive",
                30,
                6,
                2.5,
                summaries
        );

        when(podcastService.getPodcastStats()).thenReturn(mockStats);

        // Act
        PodcastStats result = podcastTools.getPodcastStats();

        // Assert
        assertNotNull(result);
        assertEquals(2, result.totalShows());
        assertEquals(150, result.totalEpisodes());
        assertEquals(LocalDateTime.of(2024, 1, 15, 0, 0), result.latestEpisodeDate());
        assertEquals("Episode 100: Spring AI Deep Dive", result.latestEpisodeTitle());
        assertEquals(30, result.episodesThisYear());
        assertEquals(6, result.episodesThisMonth());
        assertEquals(2.5, result.averageEpisodesPerMonth());
        assertEquals(2, result.showSummaries().size());
        verify(podcastService).getPodcastStats();
    }

    @Test
    void getPodcastStats_WithNoEpisodes_ShouldReturnEmptyStats() {
        // Arrange
        PodcastStats emptyStats = new PodcastStats(2, 0, null, null, 0, 0, 0.0, List.of());
        when(podcastService.getPodcastStats()).thenReturn(emptyStats);

        // Act
        PodcastStats result = podcastTools.getPodcastStats();

        // Assert
        assertNotNull(result);
        assertEquals(0, result.totalEpisodes());
        assertFalse(result.hasEpisodes());
        verify(podcastService).getPodcastStats();
    }

    @Test
    void getPodcastStats_WhenServiceThrowsException_ShouldPropagateException() {
        // Arrange
        when(podcastService.getPodcastStats()).thenThrow(new RuntimeException("Stats calculation error"));

        // Act & Assert
        RuntimeException exception = assertThrows(RuntimeException.class, () -> {
            podcastTools.getPodcastStats();
        });
        assertEquals("Stats calculation error", exception.getMessage());
    }

    @Test
    void parseCount_WithValidNumbers_ShouldReturnParsedValue() {
        // Test via getLatestEpisodes which uses parseCount internally
        when(podcastService.getLatestEpisodes(25, null)).thenReturn(List.of());

        podcastTools.getLatestEpisodes("25", null);

        verify(podcastService).getLatestEpisodes(25, null);
    }

    @Test
    void parseCount_WithExceedsMax_ShouldCapAtMaxValue() {
        // Test via getLatestEpisodes which caps at 50
        when(podcastService.getLatestEpisodes(50, null)).thenReturn(List.of());

        podcastTools.getLatestEpisodes("100", null);

        verify(podcastService).getLatestEpisodes(50, null);
    }

    @Test
    void parseCount_WithNegativeValue_ShouldUseMinimum() {
        // Test via getLatestEpisodes which has minimum of 1
        when(podcastService.getLatestEpisodes(1, null)).thenReturn(List.of());

        podcastTools.getLatestEpisodes("-5", null);

        verify(podcastService).getLatestEpisodes(1, null);
    }
}
