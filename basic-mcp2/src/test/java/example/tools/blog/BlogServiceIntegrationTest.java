package example.tools.blog;

import example.config.BlogProperties;
import example.tools.blog.model.BlogPost;
import example.tools.blog.model.BlogStats;
import org.junit.jupiter.api.Test;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.TestPropertySource;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Integration tests for BlogService with real RSS feed.
 * These tests make real HTTP calls to fetch the RSS feed.
 * YouTube properties are required because @SpringBootTest loads the full application context.
 */
@SpringBootTest
@TestPropertySource(properties = {
    "spring.ai.anthropic.api-key=test-key",
    "dvaas.blog.rss-url=https://www.danvega.dev/rss.xml",
    "dvaas.blog.cache-duration=PT30M",
    // YouTube properties required for full application context
    "dvaas.youtube.api-key=test-youtube-api-key-1234567890",
    "dvaas.youtube.channel-id=UC1234567890123456789012",
    "dvaas.youtube.application-name=dvaas-test-youtube",
    // Other services required for full application context
    "dvaas.speaking.api-url=https://www.danvega.dev/api/speaking",
    "dvaas.newsletter.api-key=test-newsletter-api-key",
    "dvaas.newsletter.publications.danvega=pub_test_123",
    "dvaas.podcast.api-key=test-podcast-api-key-1234567890"
})
class BlogServiceIntegrationTest {

    private final BlogService blogService;

    public BlogServiceIntegrationTest() {
        BlogProperties blogProperties = new BlogProperties("https://www.danvega.dev/rss.xml", Duration.ofMinutes(30));
        this.blogService = new BlogService(blogProperties);
    }

    @Test
    void getAllPosts_ShouldFetchRealRssData() {
        List<BlogPost> posts = blogService.getAllPosts();

        assertNotNull(posts);
        assertFalse(posts.isEmpty(), "Should fetch posts from real RSS feed");

        // Verify post structure
        BlogPost firstPost = posts.get(0);
        assertNotNull(firstPost.title());
        assertNotNull(firstPost.link());
        assertNotNull(firstPost.publishedAt());
        assertTrue(firstPost.getFullUrl().startsWith("https://www.danvega.dev"));

        System.out.println("Fetched " + posts.size() + " posts from RSS feed");
        System.out.println("Latest post: " + firstPost.title() + " (" + firstPost.publishedAt() + ")");
    }

    @Test
    void getLatestPosts_ShouldReturnRecentPosts() {
        List<BlogPost> posts = blogService.getLatestPosts(5);

        assertNotNull(posts);
        assertTrue(posts.size() <= 5);

        if (!posts.isEmpty()) {
            // Verify posts are sorted by date (newest first)
            for (int i = 0; i < posts.size() - 1; i++) {
                LocalDateTime current = posts.get(i).publishedAt();
                LocalDateTime next = posts.get(i + 1).publishedAt();
                assertFalse(current.isBefore(next), "Posts should be sorted by date descending");
            }

            System.out.println("Latest posts test passed with " + posts.size() + " posts");
        }
    }

    @Test
    void searchPostsByKeyword_ShouldFindSpringPosts() {
        List<BlogPost> posts = blogService.searchPostsByKeyword("spring", 10);

        assertNotNull(posts);

        if (!posts.isEmpty()) {
            assertTrue(posts.size() > 0);

            // Verify that found posts actually contain the keyword
            for (BlogPost post : posts) {
                String title = post.title().toLowerCase();
                String description = post.description() != null ? post.description().toLowerCase() : "";
                assertTrue(title.contains("spring") || description.contains("spring"),
                    "Post should contain 'spring' in title or description: " + post.title());
            }

            System.out.println("Found " + posts.size() + " posts matching 'spring'");
        } else {
            System.out.println("No posts found for 'spring' keyword");
        }
    }

    @Test
    void searchPostsByKeyword_WithUncommonKeyword_ShouldHandleNoResults() {
        List<BlogPost> posts = blogService.searchPostsByKeyword("xyzuncommonkeyword123", 10);

        assertNotNull(posts);
        assertTrue(posts.isEmpty());
    }

    @Test
    void getPostsByYear_ShouldReturnPostsFromSpecificYear() {
        int currentYear = LocalDateTime.now().getYear();
        List<BlogPost> posts = blogService.getPostsByYear(currentYear, 20);

        assertNotNull(posts);

        if (!posts.isEmpty()) {
            // Verify all posts are from the specified year
            for (BlogPost post : posts) {
                assertEquals(currentYear, post.publishedAt().getYear(),
                    "Post should be from " + currentYear + ": " + post.title());
            }

            System.out.println("Found " + posts.size() + " posts from " + currentYear);
        } else {
            System.out.println("No posts found for year " + currentYear);
        }
    }

    @Test
    void getBlogStats_ShouldProvideAccurateStatistics() {
        BlogStats stats = blogService.getBlogStats();

        assertNotNull(stats);
        assertTrue(stats.totalPosts() > 0, "Should have posts in the blog");
        assertNotNull(stats.firstPostDate());
        assertNotNull(stats.latestPostDate());
        assertFalse(stats.firstPostDate().isAfter(stats.latestPostDate()),
            "First post should be before or equal to latest post");

        assertTrue(stats.averagePostsPerMonth() >= 0);
        assertTrue(stats.postsThisYear() >= 0);
        assertTrue(stats.postsThisMonth() >= 0);
        assertTrue(stats.postsWithYouTubeVideos() >= 0);

        // Print statistics for manual verification
        System.out.println("=== Blog Statistics ===");
        System.out.println("Total Posts: " + stats.totalPosts());
        System.out.println("Blog Timespan: " + stats.getBlogTimespan());
        System.out.println("Posting Frequency: " + stats.getPostingFrequency());
        System.out.println("Average Posts/Month: " + stats.averagePostsPerMonth());
        System.out.println("Posts This Year: " + stats.postsThisYear());
        System.out.println("Posts This Month: " + stats.postsThisMonth());
        System.out.println("Posts with YouTube: " + stats.postsWithYouTubeVideos() + " (" +
                         stats.getYouTubeIntegrationPercentage() + ")");
        if (stats.mostCommonTag() != null) {
            System.out.println("Most Common Tag: " + stats.mostCommonTag());
        }
        System.out.println("First Post: " + stats.firstPostDate());
        System.out.println("Latest Post: " + stats.latestPostDate());
    }

    @Test
    void caching_ShouldWorkCorrectly() {
        // First call - should fetch from RSS
        long startTime1 = System.currentTimeMillis();
        List<BlogPost> posts1 = blogService.getAllPosts();
        long duration1 = System.currentTimeMillis() - startTime1;

        // Second call - should use cache (should be faster)
        long startTime2 = System.currentTimeMillis();
        List<BlogPost> posts2 = blogService.getAllPosts();
        long duration2 = System.currentTimeMillis() - startTime2;

        assertNotNull(posts1);
        assertNotNull(posts2);
        assertEquals(posts1.size(), posts2.size(), "Cached results should match fresh results");

        // Cache should be significantly faster (though this is environment-dependent)
        System.out.println("First call (RSS fetch): " + duration1 + "ms");
        System.out.println("Second call (cached): " + duration2 + "ms");

        // Basic sanity check - cached call should be at least somewhat faster
        assertTrue(duration2 <= duration1 + 100, "Cached call should be faster or similar");
    }

    @Test
    void tagExtraction_ShouldIdentifyCommonTags() {
        List<BlogPost> posts = blogService.getAllPosts();
        assertNotNull(posts);

        if (!posts.isEmpty()) {
            // Look for posts that should have identifiable tags
            long postsWithTags = posts.stream()
                .filter(post -> !post.extractPotentialTags().isEmpty())
                .count();

            assertTrue(postsWithTags > 0, "Should find some posts with identifiable tech tags");

            // Print some examples for verification
            posts.stream()
                .filter(post -> !post.extractPotentialTags().isEmpty())
                .limit(5)
                .forEach(post -> {
                    System.out.println("Post: " + post.title());
                    System.out.println("Tags: " + post.extractPotentialTags());
                    System.out.println();
                });
        }
    }

    @Test
    void youtubeVideoExtraction_ShouldDetectVideoUrls() {
        List<BlogPost> posts = blogService.getAllPosts();
        assertNotNull(posts);

        if (!posts.isEmpty()) {
            long postsWithVideos = posts.stream()
                .filter(BlogPost::hasYouTubeVideo)
                .count();

            System.out.println("Posts with YouTube videos: " + postsWithVideos + " out of " + posts.size());

            // If there are posts with videos, verify the URLs are valid
            posts.stream()
                .filter(BlogPost::hasYouTubeVideo)
                .limit(3)
                .forEach(post -> {
                    System.out.println("Post with video: " + post.title());
                    System.out.println("Video URL: " + post.youtubeVideoUrl());
                    assertTrue(post.youtubeVideoUrl().contains("youtube") ||
                             post.youtubeVideoUrl().contains("youtu.be"),
                             "Should contain valid YouTube URL pattern");
                    System.out.println();
                });
        }
    }
}