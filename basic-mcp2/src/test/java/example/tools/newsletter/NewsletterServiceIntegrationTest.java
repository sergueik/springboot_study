package example.tools.newsletter;

import example.config.NewsletterProperties;
import example.tools.newsletter.model.Post;
import example.tools.newsletter.model.PostSearchResult;
import example.tools.newsletter.model.PostStats;
import example.tools.newsletter.model.PublicationStats;
import org.junit.jupiter.api.Test;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

import static org.assertj.core.api.Assertions.assertThat;
import static org.assertj.core.api.Assertions.assertThatThrownBy;

/**
 * Integration tests for Beehiiv model classes and utilities.
 * These tests verify the behavior of Post, PostStats, PostSearchResult, PublicationStats, and NewsletterProperties.
 */
class NewsletterServiceIntegrationTest {

    @Test
    void testNewsletterPropertiesConfiguration() {
        Map<String, String> publications = Map.of(
                "danvega", "pub_123",
                "bytesizedai", "pub_456"
        );

        NewsletterProperties properties = new NewsletterProperties(
                "test-api-key",
                "https://api.beehiiv.com/v2",
                Duration.ofMinutes(30),
                publications
        );

        assertThat(properties.apiKey()).isEqualTo("test-api-key");
        assertThat(properties.baseUrl()).isEqualTo("https://api.beehiiv.com/v2");
        assertThat(properties.getCacheDurationMinutes()).isEqualTo(30);
        assertThat(properties.isEnabled()).isTrue();
        assertThat(properties.hasPublication("danvega")).isTrue();
        assertThat(properties.hasPublication("bytesizedai")).isTrue();
        assertThat(properties.getPublicationId("danvega")).isEqualTo("pub_123");
    }

    @Test
    void testNewsletterPropertiesValidation() {
        Map<String, String> publications = Map.of("danvega", "pub_123");

        // Test invalid cache duration
        assertThatThrownBy(() -> new NewsletterProperties(
                "api-key",
                "https://api.beehiiv.com/v2",
                Duration.ofSeconds(30),
                publications
        )).isInstanceOf(IllegalArgumentException.class)
          .hasMessageContaining("at least 1 minute");

        // Test empty publications
        assertThatThrownBy(() -> new NewsletterProperties(
                "api-key",
                "https://api.beehiiv.com/v2",
                Duration.ofMinutes(30),
                Map.of()
        )).isInstanceOf(IllegalArgumentException.class)
          .hasMessageContaining("at least one publication");
    }

    @Test
    void testPostBasicFunctionality() {
        LocalDateTime publishDate = LocalDateTime.of(2024, 12, 1, 10, 0);

        Post post = new Post(
                "post_123",
                "pub_123",
                "danvega",
                "Spring Boot 3.5 Release",
                List.of("Dan Vega", "Josh Long"),
                "confirmed",
                publishDate,
                publishDate,
                "https://danvega.dev/newsletter/spring-boot-35",
                "https://example.com/thumb.jpg",
                "Spring Boot 3.5 has been released with exciting new features...",
                "both",
                "free",
                List.of("spring", "java", "boot"),
                new PostStats(1000, 50, 800, 40)
        );

        assertThat(post.id()).isEqualTo("post_123");
        assertThat(post.publicationName()).isEqualTo("danvega");
        assertThat(post.title()).isEqualTo("Spring Boot 3.5 Release");
        assertThat(post.isPublished()).isTrue();
        assertThat(post.isDraft()).isFalse();
        assertThat(post.isArchived()).isFalse();
        assertThat(post.getAuthorsFormatted()).isEqualTo("Dan Vega, Josh Long");
        assertThat(post.getEffectivePublishDate()).isEqualTo(publishDate);
    }

    @Test
    void testPostStatusChecks() {
        Post confirmedPost = Post.basic(
                "1", "pub_123", "danvega", "Title", List.of("Dan Vega"),
                "confirmed", LocalDateTime.now(), "https://example.com"
        );
        assertThat(confirmedPost.isPublished()).isTrue();
        assertThat(confirmedPost.isDraft()).isFalse();

        Post draftPost = Post.basic(
                "2", "pub_123", "danvega", "Draft Title", List.of("Dan Vega"),
                "draft", null, null
        );
        assertThat(draftPost.isDraft()).isTrue();
        assertThat(draftPost.isPublished()).isFalse();

        Post archivedPost = Post.basic(
                "3", "pub_123", "danvega", "Archived Title", List.of("Dan Vega"),
                "archived", LocalDateTime.now().minusYears(1), "https://example.com"
        );
        assertThat(archivedPost.isArchived()).isTrue();
        assertThat(archivedPost.isPublished()).isFalse();
    }

    @Test
    void testPostStatsCalculations() {
        PostStats stats = new PostStats(1200, 75, 1000, 60);

        assertThat(stats.hasStats()).isTrue();
        assertThat(stats.opens()).isEqualTo(1200);
        assertThat(stats.clicks()).isEqualTo(75);
        assertThat(stats.uniqueOpens()).isEqualTo(1000);
        assertThat(stats.uniqueClicks()).isEqualTo(60);

        // Test rate calculations with 5000 recipients
        assertThat(stats.getOpenRate(5000)).isEqualTo(20.0); // 1000 / 5000 * 100
        assertThat(stats.getClickRate(5000)).isEqualTo(1.2); // 60 / 5000 * 100
        assertThat(stats.getClickToOpenRate()).isEqualTo(6.0); // 60 / 1000 * 100

        // Test with zero recipients
        assertThat(stats.getOpenRate(0)).isEqualTo(0.0);
        assertThat(stats.getClickRate(0)).isEqualTo(0.0);
    }

    @Test
    void testPostStatsEmpty() {
        PostStats emptyStats = PostStats.empty();

        assertThat(emptyStats.hasStats()).isFalse();
        assertThat(emptyStats.opens()).isEqualTo(0);
        assertThat(emptyStats.clicks()).isEqualTo(0);
        assertThat(emptyStats.uniqueOpens()).isEqualTo(0);
        assertThat(emptyStats.uniqueClicks()).isEqualTo(0);
        assertThat(emptyStats.getClickToOpenRate()).isEqualTo(0.0);
    }

    @Test
    void testPostSearchResultFunctionality() {
        List<Post> posts = List.of(
                Post.basic("1", "pub_123", "danvega", "Post 1", List.of("Dan Vega"),
                        "confirmed", LocalDateTime.now(), "https://example.com/1"),
                Post.basic("2", "pub_123", "danvega", "Post 2", List.of("Dan Vega"),
                        "confirmed", LocalDateTime.now(), "https://example.com/2")
        );

        PostSearchResult keywordResult = PostSearchResult.forKeyword(posts, "spring", "all");
        assertThat(keywordResult.hasResults()).isTrue();
        assertThat(keywordResult.getResultCount()).isEqualTo(2);
        assertThat(keywordResult.searchCriteria()).isEqualTo("keyword: spring");
        assertThat(keywordResult.publicationFilter()).isEqualTo("all");

        PostSearchResult statusResult = PostSearchResult.forStatus(posts, "confirmed", "danvega");
        assertThat(statusResult.hasResults()).isTrue();
        assertThat(statusResult.searchCriteria()).isEqualTo("status: confirmed");
        assertThat(statusResult.publicationFilter()).isEqualTo("danvega");

        PostSearchResult latestResult = PostSearchResult.forLatest(posts, "all");
        assertThat(latestResult.hasResults()).isTrue();
        assertThat(latestResult.searchCriteria()).isEqualTo("latest posts");

        PostSearchResult emptyResult = PostSearchResult.forKeyword(List.of(), "nonexistent", "all");
        assertThat(emptyResult.hasResults()).isFalse();
        assertThat(emptyResult.getResultCount()).isEqualTo(0);
    }

    @Test
    void testPublicationStatsBasic() {
        LocalDateTime createdAt = LocalDateTime.of(2020, 1, 1, 0, 0);

        PublicationStats basicStats = PublicationStats.basic(
                "pub_123",
                "danvega",
                100,
                createdAt
        );

        assertThat(basicStats.publicationId()).isEqualTo("pub_123");
        assertThat(basicStats.name()).isEqualTo("danvega");
        assertThat(basicStats.totalPosts()).isEqualTo(100);
        assertThat(basicStats.createdAt()).isEqualTo(createdAt);
    }

    @Test
    void testPublicationStatsCalculations() {
        PublicationStats stats = new PublicationStats(
                "pub_123",
                "danvega",
                120,      // totalPosts
                100,      // publishedPosts
                20,       // draftPosts
                15000L,   // activeSubscribers
                12000L,   // freeSubscribers
                3000L,    // premiumSubscribers
                48.5,     // averageOpenRate
                15.2,     // averageClickRate
                10000L,   // totalEmailsSent
                4850L,    // totalUniqueOpens
                1520L,    // totalClicks
                LocalDateTime.of(2020, 1, 1, 0, 0)
        );

        assertThat(stats.getPublishedPercentage()).isCloseTo(83.33, org.assertj.core.data.Offset.offset(0.01)); // 100/120 * 100
        assertThat(stats.getDraftPercentage()).isCloseTo(16.67, org.assertj.core.data.Offset.offset(0.01)); // 20/120 * 100
        assertThat(stats.getPremiumSubscriberPercentage()).isCloseTo(20.0, org.assertj.core.data.Offset.offset(0.01)); // 3000/15000 * 100
        assertThat(stats.getEngagementScore()).isCloseTo(31.85, org.assertj.core.data.Offset.offset(0.01)); // (48.5 + 15.2) / 2
    }

    @Test
    void testPublicationStatsWithZeroValues() {
        PublicationStats stats = new PublicationStats(
                "pub_123",
                "danvega",
                0, 0, 0, 0L, 0L, 0L,
                0.0, 0.0, 0L, 0L, 0L,
                LocalDateTime.of(2020, 1, 1, 0, 0)
        );

        assertThat(stats.getPublishedPercentage()).isEqualTo(0.0);
        assertThat(stats.getDraftPercentage()).isEqualTo(0.0);
        assertThat(stats.getPremiumSubscriberPercentage()).isEqualTo(0.0);
        assertThat(stats.getEngagementScore()).isEqualTo(0.0);
    }

    @Test
    void testPostEffectiveDateLogic() {
        LocalDateTime publishDate = LocalDateTime.of(2024, 12, 1, 10, 0);
        LocalDateTime displayedDate = LocalDateTime.of(2024, 12, 1, 12, 0);

        // Post with both dates - should prefer displayedDate
        Post postWithBothDates = new Post(
                "1", "pub_123", "danvega", "Title", List.of("Dan"),
                "confirmed", publishDate, displayedDate, "https://example.com",
                null, null, "both", "free", List.of(), null
        );
        assertThat(postWithBothDates.getEffectivePublishDate()).isEqualTo(displayedDate);

        // Post with only publishDate
        Post postWithPublishDate = new Post(
                "2", "pub_123", "danvega", "Title", List.of("Dan"),
                "confirmed", publishDate, null, "https://example.com",
                null, null, "both", "free", List.of(), null
        );
        assertThat(postWithPublishDate.getEffectivePublishDate()).isEqualTo(publishDate);
    }

    @Test
    void testPostAuthorsFormatting() {
        Post singleAuthor = Post.basic(
                "1", "pub_123", "danvega", "Title",
                List.of("Dan Vega"), "confirmed",
                LocalDateTime.now(), "https://example.com"
        );
        assertThat(singleAuthor.getAuthorsFormatted()).isEqualTo("Dan Vega");

        Post multipleAuthors = Post.basic(
                "2", "pub_123", "danvega", "Title",
                List.of("Dan Vega", "Josh Long", "Mark Heckler"), "confirmed",
                LocalDateTime.now(), "https://example.com"
        );
        assertThat(multipleAuthors.getAuthorsFormatted()).isEqualTo("Dan Vega, Josh Long, Mark Heckler");

        Post noAuthors = Post.basic(
                "3", "pub_123", "danvega", "Title",
                List.of(), "confirmed",
                LocalDateTime.now(), "https://example.com"
        );
        assertThat(noAuthors.getAuthorsFormatted()).isEqualTo("Unknown");
    }
}
