package example.tools.newsletter;

import example.tools.newsletter.model.Post;
import example.tools.newsletter.model.PostStats;
import example.tools.newsletter.model.PublicationStats;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.bean.override.mockito.MockitoBean;

import java.time.LocalDateTime;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.when;

/**
 * Unit tests for NewsletterTools MCP functionality with mocked NewsletterService.
 */
@SpringBootTest
@ActiveProfiles("test")
class NewsletterToolsTest {

    @Autowired
    private NewsletterTools newsletterTools;

    @MockitoBean
    private NewsletterService newsletterService;

    @Test
    void testGetLatestPostsWithDefaultParameters() {
        // Given - mock latest posts
        List<Post> mockPosts = List.of(
                new Post(
                        "post_1",
                        "pub_123",
                        "danvega",
                        "Spring Boot 3.5 Release",
                        List.of("Dan Vega"),
                        "confirmed",
                        LocalDateTime.of(2024, 12, 1, 10, 0),
                        LocalDateTime.of(2024, 12, 1, 10, 0),
                        "https://danvega.dev/newsletter/spring-boot-35",
                        "https://example.com/thumb1.jpg",
                        "Spring Boot 3.5 has been released...",
                        "both",
                        "free",
                        List.of("spring", "java"),
                        new PostStats(1000, 50, 800, 40)
                ),
                new Post(
                        "post_2",
                        "pub_456",
                        "bytesizedai",
                        "AI Weekly Update",
                        List.of("Dan Vega"),
                        "confirmed",
                        LocalDateTime.of(2024, 11, 28, 9, 0),
                        LocalDateTime.of(2024, 11, 28, 9, 0),
                        "https://bytesizedai.dev/newsletter/ai-weekly",
                        "https://example.com/thumb2.jpg",
                        "Latest AI developments...",
                        "both",
                        "free",
                        List.of("ai", "machine-learning"),
                        new PostStats(500, 25, 400, 20)
                )
        );

        when(newsletterService.getLatestPosts("all", 10)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.getLatestPosts(null, null);

        // Then
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Spring Boot 3.5 Release", result.get(0).title());
        assertEquals("AI Weekly Update", result.get(1).title());
    }

    @Test
    void testGetLatestPostsForSpecificPublication() {
        // Given - mock posts for danvega publication
        List<Post> mockPosts = List.of(
                new Post(
                        "post_1",
                        "pub_123",
                        "danvega",
                        "Spring Boot 3.5 Release",
                        List.of("Dan Vega"),
                        "confirmed",
                        LocalDateTime.of(2024, 12, 1, 10, 0),
                        LocalDateTime.of(2024, 12, 1, 10, 0),
                        "https://danvega.dev/newsletter/spring-boot-35",
                        null,
                        "Spring Boot 3.5 has been released...",
                        "both",
                        "free",
                        List.of("spring"),
                        null
                )
        );

        when(newsletterService.getLatestPosts("danvega", 10)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.getLatestPosts("danvega", null);

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("danvega", result.get(0).publicationName());
    }

    @Test
    void testGetLatestPostsWithCustomCount() {
        // Given
        List<Post> mockPosts = List.of(
                Post.basic("1", "pub_123", "danvega", "Post 1", List.of("Dan Vega"), "confirmed",
                        LocalDateTime.now(), "https://example.com/1"),
                Post.basic("2", "pub_123", "danvega", "Post 2", List.of("Dan Vega"), "confirmed",
                        LocalDateTime.now(), "https://example.com/2"),
                Post.basic("3", "pub_123", "danvega", "Post 3", List.of("Dan Vega"), "confirmed",
                        LocalDateTime.now(), "https://example.com/3")
        );

        when(newsletterService.getLatestPosts("all", 3)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.getLatestPosts(null, "3");

        // Then
        assertNotNull(result);
        assertEquals(3, result.size());
    }

    @Test
    void testGetLatestPostsEmpty() {
        // Given - no posts
        when(newsletterService.getLatestPosts(anyString(), anyInt())).thenReturn(List.of());

        // When
        List<Post> result = newsletterTools.getLatestPosts("all", "10");

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testSearchPostsByKeyword() {
        // Given - mock search results
        List<Post> mockPosts = List.of(
                new Post(
                        "post_1",
                        "pub_123",
                        "danvega",
                        "Spring Boot 3.5 Release",
                        List.of("Dan Vega"),
                        "confirmed",
                        LocalDateTime.of(2024, 12, 1, 10, 0),
                        LocalDateTime.of(2024, 12, 1, 10, 0),
                        "https://danvega.dev/newsletter/spring-boot-35",
                        null,
                        "Spring Boot 3.5 features...",
                        "both",
                        "free",
                        List.of("spring"),
                        null
                )
        );

        when(newsletterService.searchPostsByKeyword("all", "spring", 10)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.searchPostsByKeyword("spring", null, null);

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(result.get(0).title().contains("Spring"));
    }

    @Test
    void testSearchPostsByKeywordWithPublication() {
        // Given
        List<Post> mockPosts = List.of(
                Post.basic("1", "pub_456", "bytesizedai", "AI Weekly", List.of("Dan Vega"), "confirmed",
                        LocalDateTime.now(), "https://example.com/1")
        );

        when(newsletterService.searchPostsByKeyword("bytesizedai", "ai", 10)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.searchPostsByKeyword("ai", "bytesizedai", null);

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("bytesizedai", result.get(0).publicationName());
    }

    @Test
    void testSearchPostsByKeywordNoResults() {
        // Given - no results
        when(newsletterService.searchPostsByKeyword(anyString(), anyString(), anyInt())).thenReturn(List.of());

        // When
        List<Post> result = newsletterTools.searchPostsByKeyword("nonexistent", null, null);

        // Then
        assertNotNull(result);
        assertTrue(result.isEmpty());
    }

    @Test
    void testSearchPostsByKeywordWithNullKeyword() {
        // When & Then - should throw exception
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            newsletterTools.searchPostsByKeyword(null, null, null);
        });
        assertEquals("Keyword parameter is required.", exception.getMessage());
    }

    @Test
    void testSearchPostsByKeywordWithEmptyKeyword() {
        // When & Then - should throw exception
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            newsletterTools.searchPostsByKeyword("", null, null);
        });
        assertEquals("Keyword parameter is required.", exception.getMessage());
    }

    @Test
    void testGetPostsByStatusConfirmed() {
        // Given - mock confirmed posts
        List<Post> mockPosts = List.of(
                Post.basic("1", "pub_123", "danvega", "Published Post", List.of("Dan Vega"), "confirmed",
                        LocalDateTime.now(), "https://example.com/1")
        );

        when(newsletterService.getPostsByStatus("all", "confirmed", 10)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.getPostsByStatus(null, null, null);

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(result.get(0).isPublished());
    }

    @Test
    void testGetPostsByStatusDraft() {
        // Given - mock draft posts
        List<Post> mockPosts = List.of(
                Post.basic("1", "pub_123", "danvega", "Draft Post", List.of("Dan Vega"), "draft",
                        null, null)
        );

        when(newsletterService.getPostsByStatus("all", "draft", 10)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.getPostsByStatus("draft", null, null);

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(result.get(0).isDraft());
    }

    @Test
    void testGetPostsByStatusArchived() {
        // Given - mock archived posts
        List<Post> mockPosts = List.of(
                Post.basic("1", "pub_123", "danvega", "Archived Post", List.of("Dan Vega"), "archived",
                        LocalDateTime.now().minusYears(1), "https://example.com/1")
        );

        when(newsletterService.getPostsByStatus("all", "archived", 10)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.getPostsByStatus("archived", null, null);

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertTrue(result.get(0).isArchived());
    }

    @Test
    void testGetPostsByStatusWithPublication() {
        // Given
        List<Post> mockPosts = List.of(
                Post.basic("1", "pub_456", "bytesizedai", "Post", List.of("Dan Vega"), "confirmed",
                        LocalDateTime.now(), "https://example.com/1")
        );

        when(newsletterService.getPostsByStatus("bytesizedai", "confirmed", 10)).thenReturn(mockPosts);

        // When
        List<Post> result = newsletterTools.getPostsByStatus("confirmed", "bytesizedai", null);

        // Then
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("bytesizedai", result.get(0).publicationName());
    }

    @Test
    void testGetPublicationStats() {
        // Given - mock publication stats
        PublicationStats mockStats = new PublicationStats(
                "pub_123",
                "danvega",
                50,
                45,
                5,
                10000L,
                8000L,
                2000L,
                45.5,
                12.3,
                5000L,
                2275L,
                615L,
                LocalDateTime.of(2020, 1, 1, 0, 0)
        );

        when(newsletterService.getPublicationStats("all")).thenReturn(mockStats);

        // When
        PublicationStats result = newsletterTools.getPublicationStats(null);

        // Then
        assertNotNull(result);
        assertEquals(50, result.totalPosts());
        assertEquals(45, result.publishedPosts());
        assertEquals(5, result.draftPosts());
        assertEquals(10000L, result.activeSubscribers());
    }

    @Test
    void testGetPublicationStatsForSpecificPublication() {
        // Given
        PublicationStats mockStats = PublicationStats.basic(
                "pub_456",
                "bytesizedai",
                25,
                LocalDateTime.of(2022, 6, 1, 0, 0)
        );

        when(newsletterService.getPublicationStats("bytesizedai")).thenReturn(mockStats);

        // When
        PublicationStats result = newsletterTools.getPublicationStats("bytesizedai");

        // Then
        assertNotNull(result);
        assertEquals("bytesizedai", result.name());
        assertEquals(25, result.totalPosts());
    }

    @Test
    void testParseCountWithValidNumber() {
        // Given
        when(newsletterService.getLatestPosts("all", 25)).thenReturn(List.of());

        // When
        List<Post> result = newsletterTools.getLatestPosts(null, "25");

        // Then - parseCount should use 25
        assertNotNull(result);
    }

    @Test
    void testParseCountWithInvalidNumber() {
        // Given
        when(newsletterService.getLatestPosts("all", 10)).thenReturn(List.of());

        // When
        List<Post> result = newsletterTools.getLatestPosts(null, "invalid");

        // Then - parseCount should use default of 10
        assertNotNull(result);
    }

    @Test
    void testParseCountExceedsMaximum() {
        // Given
        when(newsletterService.getLatestPosts("all", 50)).thenReturn(List.of());

        // When
        List<Post> result = newsletterTools.getLatestPosts(null, "100");

        // Then - parseCount should cap at 50
        assertNotNull(result);
    }
}
