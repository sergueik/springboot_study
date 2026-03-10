package example.tools.youtube;

import example.tools.youtube.model.ChannelStats;
import example.tools.youtube.model.Video;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.boot.test.context.TestConfiguration;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.test.context.ActiveProfiles;

import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for YouTubeService that make real API calls.
 *
 * To run these tests, set environment variables:
 * export YOUTUBE_API_KEY="your_actual_api_key"
 * export YOUTUBE_CHANNEL_ID="your_actual_channel_id"
 *
 * Then run with: mvn test -Dspring.profiles.active=integration
 */
@SpringBootTest
@ActiveProfiles("integration")
@ConditionalOnProperty(name = {"dvaas.youtube.api-key", "dvaas.youtube.channel-id"})
class YouTubeServiceIntegrationTest {

    @Autowired
    private YouTubeService youTubeService;

    @Test
    void testGetChannelStats() {
        // Test real API call to get channel statistics
        ChannelStats stats = youTubeService.getChannelStats();

        // Verify we got real data back
        assertNotNull(stats, "Channel stats should not be null");
        assertNotNull(stats.channelId(), "Channel ID should not be null");
        assertNotNull(stats.title(), "Channel title should not be null");
        assertTrue(stats.title().length() > 0, "Channel title should not be empty");
        assertTrue(stats.videoCount() >= 0, "Video count should be non-negative");
        assertTrue(stats.totalViewCount() >= 0, "Total view count should be non-negative");
        assertNotNull(stats.channelCreatedAt(), "Channel created date should not be null");

        System.out.printf("✅ Channel Stats Test Passed:%n");
        System.out.printf("   Channel: %s%n", stats.title());
        System.out.printf("   Videos: %,d%n", stats.videoCount());
        System.out.printf("   Total Views: %s%n", stats.getFormattedTotalViewCount());
        System.out.printf("   Subscribers: %s%n", stats.getFormattedSubscriberCount());
    }

    @Test
    void testGetLatestVideos() {
        // Test getting latest videos from the channel
        List<Video> videos = youTubeService.getLatestVideos(3);

        // Verify we got real videos back
        assertNotNull(videos, "Videos list should not be null");
        assertTrue(videos.size() > 0, "Should have at least one video");
        assertTrue(videos.size() <= 3, "Should not exceed requested count");

        // Verify video data structure
        Video firstVideo = videos.get(0);
        assertNotNull(firstVideo.id(), "Video ID should not be null");
        assertNotNull(firstVideo.title(), "Video title should not be null");
        assertTrue(firstVideo.title().length() > 0, "Video title should not be empty");
        assertNotNull(firstVideo.publishedAt(), "Published date should not be null");
        assertTrue(firstVideo.getYouTubeUrl().startsWith("https://www.youtube.com/"),
                  "Should be valid YouTube URL");

        System.out.printf("✅ Latest Videos Test Passed:%n");
        System.out.printf("   Retrieved %d videos%n", videos.size());
        videos.forEach(video ->
            System.out.printf("   - %s (Published: %s)%n",
                video.title(), video.publishedAt().toLocalDate())
        );
    }

    @Test
    void testSearchVideosByTopic() {
        // Test searching for videos about "spring"
        List<Video> result = youTubeService.searchVideosByTopic("spring", 3);

        // Verify we got search results
        assertNotNull(result, "Search result should not be null");

        if (!result.isEmpty()) {
            // Verify video data if results found
            Video firstVideo = result.get(0);
            assertNotNull(firstVideo.id(), "Video ID should not be null");
            assertNotNull(firstVideo.title(), "Video title should not be null");
            assertTrue(firstVideo.getYouTubeUrl().startsWith("https://www.youtube.com/"),
                      "Should be valid YouTube URL");

            System.out.printf("✅ Search Videos Test Passed:%n");
            System.out.printf("   Found %d videos for 'spring'%n", result.size());
            result.forEach(video ->
                System.out.printf("   - %s%n", video.title())
            );
        } else {
            System.out.printf("ℹ️ Search Videos Test: No results found for 'spring'%n");
        }
    }

    @Test
    void testGetTopVideos() {
        // Test getting top-performing videos
        List<Video> topVideos = youTubeService.getTopVideos(3, "recent");

        // Verify we got videos back
        assertNotNull(topVideos, "Top videos list should not be null");

        if (!topVideos.isEmpty()) {
            assertTrue(topVideos.size() <= 3, "Should not exceed requested count");

            // Verify video data structure
            Video firstVideo = topVideos.get(0);
            assertNotNull(firstVideo.id(), "Video ID should not be null");
            assertNotNull(firstVideo.title(), "Video title should not be null");
            assertTrue(firstVideo.getYouTubeUrl().startsWith("https://www.youtube.com/"),
                      "Should be valid YouTube URL");

            // Verify videos are sorted by view count (descending)
            if (topVideos.size() > 1) {
                Video first = topVideos.get(0);
                Video second = topVideos.get(1);
                assertTrue(first.viewCount() >= second.viewCount(),
                          "Videos should be sorted by view count (descending)");
            }

            System.out.printf("✅ Top Videos Test Passed:%n");
            System.out.printf("   Retrieved %d top videos%n", topVideos.size());
            topVideos.forEach(video ->
                System.out.printf("   - %s (%s views)%n",
                    video.title(), video.getFormattedViewCount())
            );
        } else {
            System.out.printf("ℹ️ Top Videos Test: No videos found%n");
        }
    }

    @TestConfiguration
    static class IntegrationTestConfig {
        // Additional test configuration can go here if needed
    }
}