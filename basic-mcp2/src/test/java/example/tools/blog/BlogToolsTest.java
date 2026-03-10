package example.tools.blog;

import example.tools.blog.model.BlogPost;
import example.tools.blog.model.BlogStats;
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
 * Unit tests for BlogTools MCP class
 */
@ExtendWith(MockitoExtension.class)
class BlogToolsTest {

    @Mock
    private BlogService blogService;

    private BlogTools blogTools;

    @BeforeEach
    void setUp() {
        blogTools = new BlogTools(blogService);
    }

    @Test
    void getLatestPosts_WithDefaultCount_ShouldReturnListOfPosts() {
        // Arrange
        List<BlogPost> mockPosts = List.of(
            new BlogPost(
                "Spring Boot 3.2 Features", "/blog/spring-boot-32", "guid1",
                "Exploring the new features in Spring Boot 3.2",
                LocalDateTime.of(2024, 1, 15, 10, 0),
                "Dan Vega", List.of("spring", "boot"), null
            ),
            new BlogPost(
                "AI with Spring Boot", "/blog/ai-spring-boot", "guid2",
                "Building AI applications with Spring Boot",
                LocalDateTime.of(2024, 1, 10, 14, 30),
                "Dan Vega", List.of("spring", "ai"), "https://youtube.com/watch?v=abc123"
            )
        );

        when(blogService.getLatestPosts(10)).thenReturn(mockPosts);

        // Act
        List<BlogPost> result = blogTools.getLatestPosts(null);

        // Assert
        assertNotNull(result);
        assertEquals(2, result.size());
        assertEquals("Spring Boot 3.2 Features", result.get(0).title());
        assertEquals("AI with Spring Boot", result.get(1).title());
        assertEquals("/blog/spring-boot-32", result.get(0).link());
        assertEquals("https://youtube.com/watch?v=abc123", result.get(1).youtubeVideoUrl());
        verify(blogService).getLatestPosts(10);
    }

    @Test
    void getLatestPosts_WithCustomCount_ShouldRespectCount() {
        // Arrange
        when(blogService.getLatestPosts(5)).thenReturn(List.of());

        // Act
        List<BlogPost> result = blogTools.getLatestPosts("5");

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(blogService).getLatestPosts(5);
    }

    @Test
    void getLatestPosts_WithInvalidCount_ShouldUseDefault() {
        // Arrange
        when(blogService.getLatestPosts(10)).thenReturn(List.of());

        // Act
        List<BlogPost> result = blogTools.getLatestPosts("invalid");

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(blogService).getLatestPosts(10);
    }

    @Test
    void getLatestPosts_WhenServiceThrowsException_ShouldPropagateException() {
        // Arrange
        when(blogService.getLatestPosts(anyInt())).thenThrow(new RuntimeException("RSS feed error"));

        // Act & Assert
        RuntimeException exception = assertThrows(RuntimeException.class, () -> {
            blogTools.getLatestPosts(null);
        });
        assertEquals("RSS feed error", exception.getMessage());
    }

    @Test
    void searchPostsByKeyword_WithValidKeyword_ShouldReturnListOfPosts() {
        // Arrange
        List<BlogPost> mockPosts = List.of(
            new BlogPost(
                "Spring Security Tutorial", "/blog/spring-security", "guid1",
                "Complete guide to Spring Security",
                LocalDateTime.of(2024, 1, 20, 9, 0),
                "Dan Vega", List.of("spring", "security"), null
            )
        );

        when(blogService.searchPostsByKeyword("spring", 10)).thenReturn(mockPosts);

        // Act
        List<BlogPost> result = blogTools.searchPostsByKeyword("spring", null);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("Spring Security Tutorial", result.get(0).title());
        verify(blogService).searchPostsByKeyword("spring", 10);
    }

    @Test
    void searchPostsByKeyword_WithNullKeyword_ShouldThrowException() {
        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            blogTools.searchPostsByKeyword(null, null);
        });
        assertEquals("Keyword parameter is required.", exception.getMessage());
        verifyNoInteractions(blogService);
    }

    @Test
    void searchPostsByKeyword_WithEmptyKeyword_ShouldThrowException() {
        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            blogTools.searchPostsByKeyword("", null);
        });
        assertEquals("Keyword parameter is required.", exception.getMessage());
        verifyNoInteractions(blogService);
    }

    @Test
    void searchPostsByKeyword_WithNoResults_ShouldReturnEmptyList() {
        // Arrange
        when(blogService.searchPostsByKeyword("nonexistent", 10)).thenReturn(List.of());

        // Act
        List<BlogPost> result = blogTools.searchPostsByKeyword("nonexistent", null);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(blogService).searchPostsByKeyword("nonexistent", 10);
    }

    @Test
    void getPostsByDateRange_WithYear_ShouldReturnListOfPosts() {
        // Arrange
        List<BlogPost> mockPosts = List.of(
            new BlogPost(
                "2024 Predictions", "/blog/2024-predictions", "guid1",
                "My predictions for 2024",
                LocalDateTime.of(2024, 1, 1, 12, 0),
                "Dan Vega", List.of(), null
            )
        );

        when(blogService.getPostsByYear(2024, 10)).thenReturn(mockPosts);

        // Act
        List<BlogPost> result = blogTools.getPostsByDateRange("2024", null);

        // Assert
        assertNotNull(result);
        assertEquals(1, result.size());
        assertEquals("2024 Predictions", result.get(0).title());
        verify(blogService).getPostsByYear(2024, 10);
    }

    @Test
    void getPostsByDateRange_WithDateRange_ShouldParseAndSearch() {
        // Arrange
        when(blogService.getPostsByDateRange(any(), any(), eq(10))).thenReturn(List.of());

        // Act
        List<BlogPost> result = blogTools.getPostsByDateRange("2023-01-01 to 2023-12-31", null);

        // Assert
        assertNotNull(result);
        assertTrue(result.isEmpty());
        verify(blogService).getPostsByDateRange(any(), any(), eq(10));
    }

    @Test
    void getPostsByDateRange_WithInvalidFormat_ShouldThrowException() {
        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            blogTools.getPostsByDateRange("invalid-format", null);
        });
        assertTrue(exception.getMessage().contains("Invalid date range format"));
    }

    @Test
    void getPostsByDateRange_WithNullDateRange_ShouldThrowException() {
        // Act & Assert
        IllegalArgumentException exception = assertThrows(IllegalArgumentException.class, () -> {
            blogTools.getPostsByDateRange(null, null);
        });
        assertTrue(exception.getMessage().contains("Date range parameter is required"));
    }

    @Test
    void getBlogStats_WithValidStats_ShouldReturnStatsObject() {
        // Arrange
        BlogStats mockStats = new BlogStats(
            150,
            LocalDateTime.of(2020, 1, 1, 0, 0),
            LocalDateTime.of(2024, 12, 31, 0, 0),
            25,
            5,
            2.5,
            30,
            "spring"
        );

        when(blogService.getBlogStats()).thenReturn(mockStats);

        // Act
        BlogStats result = blogTools.getBlogStats();

        // Assert
        assertNotNull(result);
        assertEquals(150, result.totalPosts());
        assertEquals(LocalDateTime.of(2020, 1, 1, 0, 0), result.firstPostDate());
        assertEquals(LocalDateTime.of(2024, 12, 31, 0, 0), result.latestPostDate());
        assertEquals(25, result.postsThisYear());
        assertEquals(5, result.postsThisMonth());
        assertEquals(2.5, result.averagePostsPerMonth());
        assertEquals(30, result.postsWithYouTubeVideos());
        assertEquals("spring", result.mostCommonTag());
        verify(blogService).getBlogStats();
    }

    @Test
    void getBlogStats_WithNoPosts_ShouldReturnEmptyStats() {
        // Arrange
        BlogStats emptyStats = new BlogStats(0, null, null, 0, 0, 0.0, 0, null);
        when(blogService.getBlogStats()).thenReturn(emptyStats);

        // Act
        BlogStats result = blogTools.getBlogStats();

        // Assert
        assertNotNull(result);
        assertEquals(0, result.totalPosts());
        verify(blogService).getBlogStats();
    }

    @Test
    void getBlogStats_WhenServiceThrowsException_ShouldPropagateException() {
        // Arrange
        when(blogService.getBlogStats()).thenThrow(new RuntimeException("Stats calculation error"));

        // Act & Assert
        RuntimeException exception = assertThrows(RuntimeException.class, () -> {
            blogTools.getBlogStats();
        });
        assertEquals("Stats calculation error", exception.getMessage());
    }

    @Test
    void parseCount_WithValidNumbers_ShouldReturnParsedValue() {
        // Test via getLatestPosts which uses parseCount internally
        when(blogService.getLatestPosts(25)).thenReturn(List.of());

        blogTools.getLatestPosts("25");

        verify(blogService).getLatestPosts(25);
    }

    @Test
    void parseCount_WithExceedsMax_ShouldCapAtMaxValue() {
        // Test via getLatestPosts which caps at 50
        when(blogService.getLatestPosts(50)).thenReturn(List.of());

        blogTools.getLatestPosts("100");

        verify(blogService).getLatestPosts(50);
    }

    @Test
    void parseCount_WithNegativeValue_ShouldUseMinimum() {
        // Test via getLatestPosts which has minimum of 1
        when(blogService.getLatestPosts(1)).thenReturn(List.of());

        blogTools.getLatestPosts("-5");

        verify(blogService).getLatestPosts(1);
    }
}