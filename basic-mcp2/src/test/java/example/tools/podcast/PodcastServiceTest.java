package example.tools.podcast;

import example.config.PodcastProperties;
import example.tools.podcast.model.Episode;
import example.tools.podcast.model.PodcastStats;
import example.tools.podcast.model.Show;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.mockito.junit.jupiter.MockitoExtension;
import org.springframework.test.util.ReflectionTestUtils;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;
import java.util.concurrent.ConcurrentHashMap;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Unit tests for PodcastService
 */
@ExtendWith(MockitoExtension.class)
class PodcastServiceTest {

    private PodcastService podcastService;
    private PodcastProperties podcastProperties;

    @BeforeEach
    void setUp() {
        podcastProperties = new PodcastProperties(
                "test-api-key-1234567890",
                "test-app",
                Duration.ofMinutes(30),
                "spring-office-hours-id",
                "fundamentals-id"
        );
        podcastService = new PodcastService(podcastProperties);
    }

    @Test
    void constructor_ShouldInitializeWithApiKey() {
        assertNotNull(podcastService);
    }

    @Test
    void resolveShowIdentifier_WithSpringOfficeHours_ShouldResolveToConfiguredId() {
        // Clear cache to ensure we're testing name resolution from config only
        ReflectionTestUtils.setField(podcastService, "cache", new ConcurrentHashMap<>());

        String result = podcastService.resolveShowIdentifier("Spring Office Hours");
        assertEquals("spring-office-hours-id", result);

        result = podcastService.resolveShowIdentifier("spring-office-hours");
        assertEquals("spring-office-hours-id", result);

        result = podcastService.resolveShowIdentifier("SOH");
        assertEquals("spring-office-hours-id", result);
    }

    @Test
    void resolveShowIdentifier_WithFundamentals_ShouldResolveToConfiguredId() {
        // Clear cache to ensure we're testing name resolution from config only
        ReflectionTestUtils.setField(podcastService, "cache", new ConcurrentHashMap<>());

        String result = podcastService.resolveShowIdentifier("Fundamentals of Software Engineering");
        assertEquals("fundamentals-id", result);

        result = podcastService.resolveShowIdentifier("fundamentals");
        assertEquals("fundamentals-id", result);

        result = podcastService.resolveShowIdentifier("FSE");
        assertEquals("fundamentals-id", result);
    }

    @Test
    void resolveShowIdentifier_WithNullOrEmpty_ShouldReturnNull() {
        assertNull(podcastService.resolveShowIdentifier(null));
        assertNull(podcastService.resolveShowIdentifier(""));
        assertNull(podcastService.resolveShowIdentifier("   "));
    }

    @Test
    void resolveShowIdentifier_WithUnknownName_ShouldReturnAsIs() {
        // Clear cache to ensure we're testing name resolution
        ReflectionTestUtils.setField(podcastService, "cache", new ConcurrentHashMap<>());

        String result = podcastService.resolveShowIdentifier("unknown-show");
        assertEquals("unknown-show", result);
    }

    @Test
    void getLatestEpisodes_WithLimit_ShouldRespectMaxResults() {
        // This would require more sophisticated mocking of the API calls
        // For now, we test that the method exists and handles empty results gracefully
        List<Episode> episodes = podcastService.getLatestEpisodes(5, null);
        assertNotNull(episodes);
        assertTrue(episodes.size() <= 5);
    }

    @Test
    void searchEpisodes_WithNullKeyword_ShouldReturnEmptyList() {
        List<Episode> result = podcastService.searchEpisodes(null, 10, null);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void searchEpisodes_WithEmptyKeyword_ShouldReturnEmptyList() {
        List<Episode> result = podcastService.searchEpisodes("", 10, null);
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    /**
     * Test Show model functionality
     */
    @Test
    void show_IsActive_ShouldDetectPublishedStatus() {
        Show publishedShow = new Show("1", "Test Show", "Description",
                "Author", "https://example.com", null, "published", LocalDateTime.now());
        assertTrue(publishedShow.isActive());

        Show draftShow = new Show("2", "Test Show", "Description",
                "Author", "https://example.com", null, "draft", LocalDateTime.now());
        assertFalse(draftShow.isActive());
    }

    @Test
    void show_HasArtwork_ShouldDetectArtworkUrl() {
        Show showWithArtwork = new Show("1", "Test Show", "Description",
                "Author", "https://example.com", "https://example.com/art.jpg", "published", LocalDateTime.now());
        assertTrue(showWithArtwork.hasArtwork());

        Show showWithoutArtwork = new Show("2", "Test Show", "Description",
                "Author", "https://example.com", null, "published", LocalDateTime.now());
        assertFalse(showWithoutArtwork.hasArtwork());
    }

    @Test
    void show_GetShortDescription_ShouldTruncateLongDescriptions() {
        String longDescription = "a".repeat(250);
        Show show = new Show("1", "Test Show", longDescription,
                "Author", "https://example.com", null, "published", LocalDateTime.now());

        String shortDesc = show.getShortDescription();
        assertTrue(shortDesc.length() <= 203); // 200 + "..."
        assertTrue(shortDesc.endsWith("..."));
    }

    /**
     * Test Episode model functionality
     */
    @Test
    void episode_IsPublished_ShouldDetectPublishedStatus() {
        Episode publishedEpisode = Episode.basic("1", "Test Episode", "Test Show", LocalDateTime.now());
        Episode published = new Episode("1", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), null, null, "published", null, null);
        assertTrue(published.isPublished());

        Episode draft = new Episode("2", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), null, null, "draft", null, null);
        assertFalse(draft.isPublished());
    }

    @Test
    void episode_IsScheduled_ShouldDetectScheduledStatus() {
        Episode scheduled = new Episode("1", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), null, null, "scheduled", null, null);
        assertTrue(scheduled.isScheduled());

        Episode published = new Episode("2", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), null, null, "published", null, null);
        assertFalse(published.isScheduled());
    }

    @Test
    void episode_HasAudio_ShouldDetectAudioUrl() {
        Episode withAudio = new Episode("1", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), "https://example.com/audio.mp3", null, "published", null, null);
        assertTrue(withAudio.hasAudio());

        Episode withoutAudio = new Episode("2", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), null, null, "published", null, null);
        assertFalse(withoutAudio.hasAudio());
    }

    @Test
    void episode_GetEpisodeIdentifier_ShouldFormatCorrectly() {
        Episode withSeasonAndNumber = new Episode("1", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), null, null, "published", 2, 5);
        assertEquals("S2E5", withSeasonAndNumber.getEpisodeIdentifier());

        Episode withNumberOnly = new Episode("2", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), null, null, "published", null, 10);
        assertEquals("Episode 10", withNumberOnly.getEpisodeIdentifier());

        Episode withoutNumbers = new Episode("3", "Test", "Desc", "showId", "Test Show",
                LocalDateTime.now(), null, null, "published", null, null);
        assertEquals("", withoutNumbers.getEpisodeIdentifier());
    }

    /**
     * Test PodcastStats model functionality
     */
    @Test
    void podcastStats_HasEpisodes_ShouldDetectEpisodeCount() {
        PodcastStats withEpisodes = new PodcastStats(2, 50, LocalDateTime.now(), "Latest Episode",
                10, 2, 2.5, List.of());
        assertTrue(withEpisodes.hasEpisodes());

        PodcastStats withoutEpisodes = new PodcastStats(2, 0, null, null,
                0, 0, 0.0, List.of());
        assertFalse(withoutEpisodes.hasEpisodes());
    }

    @Test
    void podcastStats_GetFormattedAverageEpisodesPerMonth_ShouldFormatCorrectly() {
        PodcastStats stats = new PodcastStats(2, 50, LocalDateTime.now(), "Latest Episode",
                10, 2, 2.567, List.of());
        assertEquals("2.6", stats.getFormattedAverageEpisodesPerMonth());
    }

    @Test
    void podcastStats_GetMostActiveShow_ShouldFindShowWithMostEpisodes() {
        List<PodcastStats.ShowSummary> summaries = List.of(
                new PodcastStats.ShowSummary("Show A", 10, LocalDateTime.now()),
                new PodcastStats.ShowSummary("Show B", 25, LocalDateTime.now()),
                new PodcastStats.ShowSummary("Show C", 15, LocalDateTime.now())
        );

        PodcastStats stats = new PodcastStats(3, 50, LocalDateTime.now(), "Latest Episode",
                10, 2, 2.5, summaries);
        assertEquals("Show B", stats.getMostActiveShow());
    }

    @Test
    void podcastStats_GetMostActiveShow_WithEmptySummaries_ShouldReturnNA() {
        PodcastStats stats = new PodcastStats(0, 0, null, null,
                0, 0, 0.0, List.of());
        assertEquals("N/A", stats.getMostActiveShow());
    }
}
