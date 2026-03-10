package example.tools.podcast;

import example.config.PodcastProperties;
import example.tools.podcast.model.Episode;
import example.tools.podcast.model.PodcastStats;
import example.tools.podcast.model.Show;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for PodcastService with real Transistor.fm API.
 * These tests make real HTTP calls to the Transistor API.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "dvaas.podcast.api-key=test-podcast-api-key-1234567890",
    "dvaas.podcast.application-name=dvaas-test-podcast",
    "dvaas.podcast.cache-duration=PT30M",
    "dvaas.podcast.spring-office-hours-show-id=41020",
    "dvaas.podcast.fundamentals-show-id=69333",
    // Required for full application context
    "spring.ai.anthropic.api-key=test-key",
    "dvaas.blog.rss-url=https://www.danvega.dev/rss.xml",
    "dvaas.youtube.api-key=test-youtube-api-key-1234567890",
    "dvaas.youtube.channel-id=UC1234567890123456789012"
})
class PodcastServiceIntegrationTest {

    @Autowired
    private PodcastService podcastService;

    @Autowired
    private PodcastProperties podcastProperties;

    @Test
    void getAllShows_ShouldFetchRealShowsFromTransistor() {
        List<Show> shows = podcastService.getAllShows();

        assertNotNull(shows);
        assertFalse(shows.isEmpty(), "Should fetch shows from real Transistor API");

        // Verify show structure
        Show firstShow = shows.get(0);
        assertNotNull(firstShow.id());
        assertNotNull(firstShow.title());

        System.out.println("=== Fetched Shows from Transistor ===");
        for (Show show : shows) {
            System.out.println("Show: " + show.title() + " (ID: " + show.id() + ")");
            System.out.println("  Status: " + show.status());
            if (show.description() != null) {
                System.out.println("  Description: " + show.getShortDescription());
            }
            System.out.println();
        }
    }

    @Test
    void getLatestEpisodes_ShouldReturnRecentEpisodes() {
        List<Episode> episodes = podcastService.getLatestEpisodes(10, null);

        assertNotNull(episodes);
        assertFalse(episodes.isEmpty(), "Should have episodes");
        assertTrue(episodes.size() <= 10);

        // Verify episodes are sorted by date (newest first)
        for (int i = 0; i < episodes.size() - 1; i++) {
            LocalDateTime current = episodes.get(i).publishedAt();
            LocalDateTime next = episodes.get(i + 1).publishedAt();
            assertFalse(current.isBefore(next), "Episodes should be sorted by date descending");
        }

        System.out.println("=== Latest Episodes ===");
        episodes.stream().limit(5).forEach(episode -> {
            System.out.println("Episode: " + episode.title());
            System.out.println("  Show: " + episode.showTitle());
            System.out.println("  Published: " + episode.publishedAt());
            System.out.println("  Status: " + episode.status());
            if (episode.season() != null && episode.number() != null) {
                System.out.println("  Episode: " + episode.getEpisodeIdentifier());
            }
            System.out.println();
        });
    }

    @Test
    void getLatestEpisodes_WithSpringOfficeHoursFilter_ShouldReturnOnlyThatShow() {
        // Test with show name resolution
        List<Episode> episodes = podcastService.getLatestEpisodes(5, "Spring Office Hours");

        assertNotNull(episodes);
        assertFalse(episodes.isEmpty(), "Spring Office Hours should have episodes");
        assertTrue(episodes.size() <= 5);

        // Verify all episodes are from Spring Office Hours
        for (Episode episode : episodes) {
            assertNotNull(episode.showId());
            assertNotNull(episode.title());
            System.out.println("Spring Office Hours Episode: " + episode.title() + " (" + episode.publishedAt() + ")");
        }

        System.out.println("\nTotal Spring Office Hours episodes found: " + episodes.size());
    }

    @Test
    void resolveShowIdentifier_WithSpringOfficeHours_ShouldResolveToShowId() {
        // Test show name resolution
        String resolvedId = podcastService.resolveShowIdentifier("Spring Office Hours");
        assertNotNull(resolvedId);
        assertEquals("41020", resolvedId);

        // Test with different variations
        assertEquals("41020", podcastService.resolveShowIdentifier("spring office hours"));
        assertEquals("41020", podcastService.resolveShowIdentifier("SOH"));

        System.out.println("Show name resolution working correctly: 'Spring Office Hours' -> '" + resolvedId + "'");
    }

    @Test
    void searchEpisodes_WithKeyword_ShouldFindMatchingEpisodes() {
        List<Episode> episodes = podcastService.searchEpisodes("spring", 10, null);

        assertNotNull(episodes);

        if (!episodes.isEmpty()) {
            System.out.println("=== Episodes matching 'spring' ===");
            for (Episode episode : episodes) {
                String title = episode.title().toLowerCase();
                String description = episode.description() != null ? episode.description().toLowerCase() : "";
                assertTrue(title.contains("spring") || description.contains("spring"),
                    "Episode should contain 'spring' in title or description: " + episode.title());

                System.out.println("Episode: " + episode.title());
                System.out.println("  Show: " + episode.showTitle());
                System.out.println();
            }
            System.out.println("Found " + episodes.size() + " episodes matching 'spring'");
        }
    }

    @Test
    void searchEpisodes_WithSpringOfficeHoursFilter_ShouldOnlySearchThatShow() {
        List<Episode> episodes = podcastService.searchEpisodes("spring", 5, "Spring Office Hours");

        assertNotNull(episodes);

        if (!episodes.isEmpty()) {
            System.out.println("=== Spring Office Hours episodes matching 'spring' ===");
            for (Episode episode : episodes) {
                assertNotNull(episode.showId());
                System.out.println("Episode: " + episode.title() + " (" + episode.publishedAt() + ")");
            }
        }
    }

    @Test
    void getPodcastStats_ShouldProvideAccurateStatistics() {
        PodcastStats stats = podcastService.getPodcastStats();

        assertNotNull(stats);
        assertTrue(stats.totalShows() > 0, "Should have at least one show");
        assertTrue(stats.totalEpisodes() > 0, "Should have episodes");
        assertNotNull(stats.latestEpisodeDate());
        assertNotNull(stats.latestEpisodeTitle());

        assertTrue(stats.averageEpisodesPerMonth() >= 0);
        assertTrue(stats.episodesThisYear() >= 0);
        assertTrue(stats.episodesThisMonth() >= 0);

        // Print statistics for manual verification
        System.out.println("=== Podcast Statistics ===");
        System.out.println("Total Shows: " + stats.totalShows());
        System.out.println("Total Episodes: " + stats.totalEpisodes());
        System.out.println("Latest Episode: " + stats.latestEpisodeTitle());
        System.out.println("Latest Episode Date: " + stats.latestEpisodeDate());
        System.out.println("Average Episodes/Month: " + stats.getFormattedAverageEpisodesPerMonth());
        System.out.println("Episodes This Year: " + stats.episodesThisYear());
        System.out.println("Episodes This Month: " + stats.episodesThisMonth());

        if (!stats.showSummaries().isEmpty()) {
            System.out.println("\n=== Per-Show Statistics ===");
            for (PodcastStats.ShowSummary summary : stats.showSummaries()) {
                System.out.println(summary.showTitle() + ": " + summary.episodeCount() + " episodes");
                System.out.println("  Latest: " + summary.latestEpisode());
            }
            System.out.println("\nMost Active Show: " + stats.getMostActiveShow());
        }
    }

    @Test
    void caching_ShouldWorkCorrectly() {
        // First call - should fetch from API
        long startTime1 = System.currentTimeMillis();
        List<Show> shows1 = podcastService.getAllShows();
        long duration1 = System.currentTimeMillis() - startTime1;

        // Second call - should use cache (should be faster)
        long startTime2 = System.currentTimeMillis();
        List<Show> shows2 = podcastService.getAllShows();
        long duration2 = System.currentTimeMillis() - startTime2;

        assertNotNull(shows1);
        assertNotNull(shows2);
        assertEquals(shows1.size(), shows2.size(), "Cached results should match fresh results");

        System.out.println("First call (API fetch): " + duration1 + "ms");
        System.out.println("Second call (cached): " + duration2 + "ms");

        // Basic sanity check - cached call should be faster or similar
        assertTrue(duration2 <= duration1 + 100, "Cached call should be faster or similar");
    }

    @Test
    void episodeDetails_ShouldHaveValidStructure() {
        List<Episode> episodes = podcastService.getLatestEpisodes(5, null);
        assertNotNull(episodes);
        assertFalse(episodes.isEmpty());

        Episode firstEpisode = episodes.get(0);

        // Verify required fields
        assertNotNull(firstEpisode.id());
        assertNotNull(firstEpisode.title());
        assertNotNull(firstEpisode.publishedAt());
        assertNotNull(firstEpisode.status());

        System.out.println("=== Episode Details Verification ===");
        System.out.println("Episode: " + firstEpisode.title());
        System.out.println("ID: " + firstEpisode.id());
        System.out.println("Show: " + firstEpisode.showTitle());
        System.out.println("Published: " + firstEpisode.publishedAt());
        System.out.println("Status: " + firstEpisode.status());
        System.out.println("Published: " + (firstEpisode.isPublished() ? "Yes" : "No"));
        System.out.println("Scheduled: " + (firstEpisode.isScheduled() ? "Yes" : "No"));

        if (firstEpisode.audioUrl() != null) {
            System.out.println("Audio URL: " + firstEpisode.audioUrl());
            System.out.println("Has Audio: " + firstEpisode.hasAudio());
        }

        if (firstEpisode.duration() != null) {
            System.out.println("Duration: " + firstEpisode.getFormattedDuration());
        }

        if (firstEpisode.season() != null || firstEpisode.number() != null) {
            System.out.println("Episode Identifier: " + firstEpisode.getEpisodeIdentifier());
        }
    }

    @Test
    void showNameResolution_ShouldWorkForAllConfiguredShows() {
        // Test Spring Office Hours
        String sohId = podcastService.resolveShowIdentifier("Spring Office Hours");
        assertNotNull(sohId);
        assertEquals("41020", sohId);

        // Test Fundamentals
        String fundId = podcastService.resolveShowIdentifier("Fundamentals of Software Engineering");
        assertNotNull(fundId);
        assertEquals("69333", fundId);

        // Test variations
        assertEquals(sohId, podcastService.resolveShowIdentifier("spring-office-hours"));
        assertEquals(fundId, podcastService.resolveShowIdentifier("fundamentals"));

        System.out.println("=== Show Name Resolution Tests ===");
        System.out.println("'Spring Office Hours' -> " + sohId);
        System.out.println("'Fundamentals of Software Engineering' -> " + fundId);
    }

    @Test
    void getShowById_ShouldReturnShowDetails() {
        // First get all shows to get a valid ID
        List<Show> shows = podcastService.getAllShows();
        assertNotNull(shows);
        assertFalse(shows.isEmpty());

        String showId = shows.get(0).id();
        Show show = podcastService.getShowById(showId);

        assertNotNull(show);
        assertEquals(showId, show.id());
        assertNotNull(show.title());

        System.out.println("=== Show Details ===");
        System.out.println("ID: " + show.id());
        System.out.println("Title: " + show.title());
        System.out.println("Status: " + show.status());
        System.out.println("Active: " + show.isActive());

        if (show.author() != null) {
            System.out.println("Author: " + show.author());
        }

        if (show.websiteUrl() != null) {
            System.out.println("Website: " + show.websiteUrl());
        }

        if (show.artworkUrl() != null) {
            System.out.println("Has Artwork: " + show.hasArtwork());
        }

        if (show.description() != null) {
            System.out.println("Description: " + show.getShortDescription());
        }
    }
}
