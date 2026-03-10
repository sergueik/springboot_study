package example.tools.newsletter;

import example.config.NewsletterProperties;
import example.tools.newsletter.model.Post;
import example.tools.newsletter.model.PublicationStats;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;

import java.time.Duration;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * Real API integration test for Beehiiv service
 * This test makes actual calls to the Beehiiv API to verify the integration works
 *
 * Only runs when BEEHIIV_API_KEY environment variable is set
 */
@EnabledIfEnvironmentVariable(named = "BEEHIIV_API_KEY", matches = ".+")
class BeehiivApiIntegrationTest {

    private NewsletterService newsletterService;
    private NewsletterProperties newsletterProperties;

    @BeforeEach
    void setUp() {
        // Load real environment variables
        String apiKey = System.getenv("BEEHIIV_API_KEY");
        String danvegaPubId = System.getenv("BEEHIIV_DANVEGA_PUBLICATION_ID");
        String bytesizedaiPubId = System.getenv("BEEHIIV_BYTESIZEDAI_PUBLICATION_ID");

        // Create properties with real credentials
        Map<String, String> publications = Map.of(
                "danvega", danvegaPubId != null ? danvegaPubId : "pub_unknown",
                "bytesizedai", bytesizedaiPubId != null ? bytesizedaiPubId : "pub_unknown"
        );

        newsletterProperties = new NewsletterProperties(
                apiKey,
                "https://api.beehiiv.com/v2",
                Duration.ofMinutes(1), // Short cache for testing
                publications
        );

        // Create service with real properties
        newsletterService = new NewsletterService(newsletterProperties);
    }

    @Test
    void testGetLatestPostsFromDanvegaPublication() {
        System.out.println("\n=== Testing: Get latest posts from danvega publication ===");

        List<Post> posts = newsletterService.getLatestPosts("danvega", 5);

        System.out.println("Retrieved " + posts.size() + " posts from danvega publication");

        assertThat(posts).isNotNull();
        assertThat(posts).isNotEmpty();

        // Verify post structure
        Post firstPost = posts.get(0);
        assertThat(firstPost.id()).isNotNull();
        assertThat(firstPost.publicationId()).isNotNull();
        assertThat(firstPost.publicationName()).isEqualTo("danvega");
        assertThat(firstPost.title()).isNotNull();

        // Print details
        posts.forEach(post -> {
            System.out.println("  - " + post.title());
            System.out.println("    ID: " + post.id());
            System.out.println("    Status: " + post.status());
            System.out.println("    Published: " + (post.isPublished() ? "Yes" : "No"));
            System.out.println("    URL: " + post.webUrl());
        });
    }

    @Test
    void testGetLatestPostsFromBytesizedaiPublication() {
        System.out.println("\n=== Testing: Get latest posts from bytesizedai publication ===");

        List<Post> posts = newsletterService.getLatestPosts("bytesizedai", 5);

        System.out.println("Retrieved " + posts.size() + " posts from bytesizedai publication");

        assertThat(posts).isNotNull();
        assertThat(posts).isNotEmpty();

        Post firstPost = posts.get(0);
        assertThat(firstPost.publicationName()).isEqualTo("bytesizedai");

        posts.forEach(post -> {
            System.out.println("  - " + post.title());
        });
    }

    @Test
    void testGetLatestPostsFromAllPublications() {
        System.out.println("\n=== Testing: Get latest posts from all publications ===");

        List<Post> posts = newsletterService.getLatestPosts("all", 10);

        System.out.println("Retrieved " + posts.size() + " posts from all publications");

        assertThat(posts).isNotNull();
        assertThat(posts).isNotEmpty();

        // Should contain posts from both publications
        boolean hasDanvega = posts.stream().anyMatch(p -> "danvega".equals(p.publicationName()));
        boolean hasBytesizedai = posts.stream().anyMatch(p -> "bytesizedai".equals(p.publicationName()));

        System.out.println("Contains danvega posts: " + hasDanvega);
        System.out.println("Contains bytesizedai posts: " + hasBytesizedai);

        assertThat(hasDanvega || hasBytesizedai).isTrue();

        // Print breakdown by publication
        Map<String, Long> postsByPublication = posts.stream()
                .collect(java.util.stream.Collectors.groupingBy(Post::publicationName, java.util.stream.Collectors.counting()));

        System.out.println("\nPosts by publication:");
        postsByPublication.forEach((pub, count) -> {
            System.out.println("  " + pub + ": " + count + " posts");
        });
    }

    @Test
    void testSearchPostsByKeyword() {
        System.out.println("\n=== Testing: Search posts by keyword ===");

        String keyword = "spring";
        List<Post> posts = newsletterService.searchPostsByKeyword("all", keyword, 10);

        System.out.println("Found " + posts.size() + " posts matching keyword '" + keyword + "'");

        assertThat(posts).isNotNull();

        if (!posts.isEmpty()) {
            posts.forEach(post -> {
                System.out.println("  - " + post.title());
                System.out.println("    Publication: " + post.publicationName());
            });
        } else {
            System.out.println("  No posts found with keyword '" + keyword + "'");
        }
    }

    @Test
    void testGetPostsByStatus() {
        System.out.println("\n=== Testing: Get posts by status ===");

        List<Post> confirmedPosts = newsletterService.getPostsByStatus("all", "confirmed", 10);

        System.out.println("Retrieved " + confirmedPosts.size() + " confirmed posts");

        assertThat(confirmedPosts).isNotNull();

        if (!confirmedPosts.isEmpty()) {
            assertThat(confirmedPosts.get(0).isPublished()).isTrue();
            System.out.println("  First confirmed post: " + confirmedPosts.get(0).title());
        }

        // Test draft posts
        List<Post> draftPosts = newsletterService.getPostsByStatus("all", "draft", 5);
        System.out.println("Retrieved " + draftPosts.size() + " draft posts");

        if (!draftPosts.isEmpty()) {
            assertThat(draftPosts.get(0).isDraft()).isTrue();
            System.out.println("  First draft post: " + draftPosts.get(0).title());
        }
    }

    @Test
    void testGetPublicationStats() {
        System.out.println("\n=== Testing: Get publication stats ===");

        PublicationStats danvegaStats = newsletterService.getPublicationStats("danvega");

        System.out.println("\nDanvega Publication Stats:");
        System.out.println("  Name: " + danvegaStats.name());
        System.out.println("  Publication ID: " + danvegaStats.publicationId());
        System.out.println("  Total Posts: " + danvegaStats.totalPosts());
        System.out.println("  Published Posts: " + danvegaStats.publishedPosts());
        System.out.println("  Draft Posts: " + danvegaStats.draftPosts());
        System.out.println("  Created At: " + danvegaStats.createdAt());

        assertThat(danvegaStats).isNotNull();
        assertThat(danvegaStats.name()).isEqualTo("danvega");
        assertThat(danvegaStats.totalPosts()).isGreaterThan(0);

        // Test bytesizedai stats
        PublicationStats bytesizedaiStats = newsletterService.getPublicationStats("bytesizedai");

        System.out.println("\nBytesizedai Publication Stats:");
        System.out.println("  Name: " + bytesizedaiStats.name());
        System.out.println("  Total Posts: " + bytesizedaiStats.totalPosts());
        System.out.println("  Published Posts: " + bytesizedaiStats.publishedPosts());

        assertThat(bytesizedaiStats).isNotNull();
        assertThat(bytesizedaiStats.name()).isEqualTo("bytesizedai");
    }

    @Test
    void testPostDataIntegrity() {
        System.out.println("\n=== Testing: Post data integrity ===");

        List<Post> posts = newsletterService.getLatestPosts("danvega", 3);

        assertThat(posts).isNotEmpty();

        Post post = posts.get(0);

        System.out.println("\nPost Data Validation:");
        System.out.println("  Title: " + post.title());
        System.out.println("  ID: " + post.id());
        System.out.println("  Publication ID: " + post.publicationId());
        System.out.println("  Publication Name: " + post.publicationName());
        System.out.println("  Authors: " + post.getAuthorsFormatted());
        System.out.println("  Status: " + post.status());
        System.out.println("  Publish Date: " + post.publishDate());
        System.out.println("  Web URL: " + post.webUrl());
        System.out.println("  Platform: " + post.platform());
        System.out.println("  Audience: " + post.audience());

        // Validate required fields
        assertThat(post.id()).isNotNull().isNotEmpty();
        assertThat(post.publicationId()).isNotNull().isNotEmpty();
        assertThat(post.publicationName()).isEqualTo("danvega");
        assertThat(post.title()).isNotNull().isNotEmpty();
        assertThat(post.status()).isNotNull().isIn("confirmed", "draft", "archived");

        if (post.stats() != null) {
            System.out.println("\n  Stats:");
            System.out.println("    Opens: " + post.stats().opens());
            System.out.println("    Clicks: " + post.stats().clicks());
            System.out.println("    Unique Opens: " + post.stats().uniqueOpens());
            System.out.println("    Unique Clicks: " + post.stats().uniqueClicks());
        }
    }
}
