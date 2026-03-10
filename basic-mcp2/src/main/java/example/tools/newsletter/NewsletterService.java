package example.tools.newsletter;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import example.config.NewsletterProperties;
import example.tools.newsletter.model.Post;
import example.tools.newsletter.model.PostStats;
import example.tools.newsletter.model.PublicationStats;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Service for interacting with the newsletter API (Beehiiv)
 * Supports multiple publications (e.g., danvega, bytesizedai)
 */
@Service
@ConditionalOnProperty(name = "dvaas.newsletter.api-key")
public class NewsletterService {

    private static final Logger logger = LoggerFactory.getLogger(NewsletterService.class);

    private final NewsletterProperties newsletterProperties;
    private final HttpClient httpClient;
    private final ObjectMapper objectMapper;
    private final Map<String, Object> cache = new ConcurrentHashMap<>();
    private final Map<String, LocalDateTime> cacheTimestamps = new ConcurrentHashMap<>();

    public NewsletterService(NewsletterProperties newsletterProperties) {
        this.newsletterProperties = newsletterProperties;
        this.httpClient = HttpClient.newHttpClient();
        this.objectMapper = new ObjectMapper();
        logger.info("Newsletter service initialized with base URL: {}", newsletterProperties.baseUrl());
        logger.info("Newsletter publications: {}", newsletterProperties.getPublicationNames());
        logger.info("Newsletter cache duration: {} minutes", newsletterProperties.getCacheDurationMinutes());
    }

    /**
     * Get latest posts from specific publication or all publications
     */
    public List<Post> getLatestPosts(String publication, int maxResults) {
        if ("all".equalsIgnoreCase(publication)) {
            return getAllPostsFromAllPublications(maxResults);
        }

        if (!newsletterProperties.hasPublication(publication)) {
            throw new IllegalArgumentException("Unknown publication: " + publication + ". Available: " + newsletterProperties.getPublicationNames());
        }

        List<Post> allPosts = getCachedPosts(publication);
        return allPosts.stream()
                .sorted((p1, p2) -> {
                    LocalDateTime date1 = p1.getEffectivePublishDate();
                    LocalDateTime date2 = p2.getEffectivePublishDate();
                    if (date1 == null && date2 == null) return 0;
                    if (date1 == null) return 1;
                    if (date2 == null) return -1;
                    return date2.compareTo(date1);
                })
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Search posts by keyword
     */
    public List<Post> searchPostsByKeyword(String publication, String keyword, int maxResults) {
        if (keyword == null || keyword.trim().isEmpty()) {
            return List.of();
        }

        String searchTerm = keyword.toLowerCase().trim();
        List<Post> allPosts = getPostsForPublication(publication);

        return allPosts.stream()
                .filter(post -> matchesKeyword(post, searchTerm))
                .sorted((p1, p2) -> {
                    LocalDateTime date1 = p1.getEffectivePublishDate();
                    LocalDateTime date2 = p2.getEffectivePublishDate();
                    if (date1 == null && date2 == null) return 0;
                    if (date1 == null) return 1;
                    if (date2 == null) return -1;
                    return date2.compareTo(date1);
                })
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Get posts by status (draft, confirmed, archived, all)
     */
    public List<Post> getPostsByStatus(String publication, String status, int maxResults) {
        List<Post> allPosts = getPostsForPublication(publication);

        if ("all".equalsIgnoreCase(status)) {
            return allPosts.stream()
                    .sorted((p1, p2) -> {
                        LocalDateTime date1 = p1.getEffectivePublishDate();
                        LocalDateTime date2 = p2.getEffectivePublishDate();
                        if (date1 == null && date2 == null) return 0;
                        if (date1 == null) return 1;
                        if (date2 == null) return -1;
                        return date2.compareTo(date1);
                    })
                    .limit(Math.min(maxResults, 50))
                    .toList();
        }

        return allPosts.stream()
                .filter(post -> status.equalsIgnoreCase(post.status()))
                .sorted((p1, p2) -> {
                    LocalDateTime date1 = p1.getEffectivePublishDate();
                    LocalDateTime date2 = p2.getEffectivePublishDate();
                    if (date1 == null && date2 == null) return 0;
                    if (date1 == null) return 1;
                    if (date2 == null) return -1;
                    return date2.compareTo(date1);
                })
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Get publication statistics
     */
    public PublicationStats getPublicationStats(String publication) {
        if (!"all".equalsIgnoreCase(publication) && !newsletterProperties.hasPublication(publication)) {
            throw new IllegalArgumentException("Unknown publication: " + publication + ". Available: " + newsletterProperties.getPublicationNames());
        }

        List<Post> posts = getPostsForPublication(publication);

        int totalPosts = posts.size();
        int publishedPosts = (int) posts.stream().filter(Post::isPublished).count();
        int draftPosts = (int) posts.stream().filter(Post::isDraft).count();

        // Calculate average metrics from posts with stats
        long postsWithStats = posts.stream()
                .filter(p -> p.stats() != null && p.stats().hasStats())
                .count();

        double avgOpenRate = 0.0;
        double avgClickRate = 0.0;
        long totalOpens = 0L;
        long totalClicks = 0L;

        if (postsWithStats > 0) {
            for (Post post : posts) {
                if (post.stats() != null && post.stats().hasStats()) {
                    totalOpens += post.stats().uniqueOpens();
                    totalClicks += post.stats().uniqueClicks();
                }
            }
        }

        LocalDateTime createdAt = posts.stream()
                .map(Post::publishDate)
                .filter(Objects::nonNull)
                .min(LocalDateTime::compareTo)
                .orElse(null);

        return new PublicationStats(
                newsletterProperties.getPublicationId(publication),
                publication,
                totalPosts,
                publishedPosts,
                draftPosts,
                0L, // Would need separate API call for subscriber counts
                0L,
                0L,
                avgOpenRate,
                avgClickRate,
                0L,
                totalOpens,
                totalClicks,
                createdAt
        );
    }

    /**
     * Get posts for a specific publication or all publications
     */
    private List<Post> getPostsForPublication(String publication) {
        if ("all".equalsIgnoreCase(publication)) {
            return getAllPostsFromAllPublications(50);
        }

        if (!newsletterProperties.hasPublication(publication)) {
            throw new IllegalArgumentException("Unknown publication: " + publication + ". Available: " + newsletterProperties.getPublicationNames());
        }

        return getCachedPosts(publication);
    }

    /**
     * Get all posts from all publications
     */
    private List<Post> getAllPostsFromAllPublications(int maxResults) {
        List<Post> allPosts = new ArrayList<>();

        for (String pubName : newsletterProperties.getPublicationNames()) {
            allPosts.addAll(getCachedPosts(pubName));
        }

        return allPosts.stream()
                .sorted((p1, p2) -> {
                    LocalDateTime date1 = p1.getEffectivePublishDate();
                    LocalDateTime date2 = p2.getEffectivePublishDate();
                    if (date1 == null && date2 == null) return 0;
                    if (date1 == null) return 1;
                    if (date2 == null) return -1;
                    return date2.compareTo(date1);
                })
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Get cached posts for a publication, refreshing if needed
     */
    @SuppressWarnings("unchecked")
    private List<Post> getCachedPosts(String publication) {
        String cacheKey = "posts_" + publication;

        if (needsCacheRefresh(cacheKey)) {
            try {
                String publicationId = newsletterProperties.getPublicationId(publication);
                List<Post> posts = fetchPostsFromApi(publicationId, publication);
                cache.put(cacheKey, posts);
                cacheTimestamps.put(cacheKey, LocalDateTime.now());
                logger.info("Newsletter cache refreshed for publication '{}' with {} posts", publication, posts.size());
                return posts;
            } catch (Exception e) {
                logger.error("Failed to refresh newsletter cache for publication '{}'", publication, e);
                return (List<Post>) cache.getOrDefault(cacheKey, List.of());
            }
        }

        return (List<Post>) cache.getOrDefault(cacheKey, List.of());
    }

    /**
     * Check if cache needs refresh
     */
    private boolean needsCacheRefresh(String cacheKey) {
        LocalDateTime lastCacheTime = cacheTimestamps.get(cacheKey);
        return lastCacheTime == null ||
               ChronoUnit.MINUTES.between(lastCacheTime, LocalDateTime.now()) >= newsletterProperties.getCacheDurationMinutes() ||
               !cache.containsKey(cacheKey);
    }

    /**
     * Fetch posts from Beehiiv API
     */
    private List<Post> fetchPostsFromApi(String publicationId, String publicationName) throws IOException, InterruptedException {
        String url = String.format("%s/publications/%s/posts?limit=50&order_by=publish_date&direction=desc",
                newsletterProperties.baseUrl(), publicationId);

        logger.info("Fetching posts from Beehiiv API for publication '{}': {}", publicationName, url);

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(url))
                .header("Authorization", "Bearer " + newsletterProperties.apiKey())
                .header("Accept", "application/json")
                .GET()
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        if (response.statusCode() != 200) {
            throw new IOException("HTTP " + response.statusCode() + ": " + response.body());
        }

        // Parse JSON response
        Map<String, Object> apiResponse = objectMapper.readValue(
            response.body(),
            new TypeReference<Map<String, Object>>() {}
        );

        @SuppressWarnings("unchecked")
        List<Map<String, Object>> postsData = (List<Map<String, Object>>) apiResponse.get("data");

        if (postsData == null) {
            logger.warn("No 'data' field in API response for publication '{}'", publicationName);
            return List.of();
        }

        List<Post> posts = new ArrayList<>();
        for (Map<String, Object> postData : postsData) {
            Post post = convertApiDataToPost(postData, publicationId, publicationName);
            if (post != null) {
                posts.add(post);
            }
        }

        logger.info("Successfully parsed {} posts from Beehiiv API for publication '{}'", posts.size(), publicationName);
        return posts;
    }

    /**
     * Convert API response data to Post object
     */
    @SuppressWarnings("unchecked")
    private Post convertApiDataToPost(Map<String, Object> data, String publicationId, String publicationName) {
        try {
            String id = getStringValue(data, "id");
            String title = getStringValue(data, "title");

            List<String> authors = new ArrayList<>();
            Object authorsObj = data.get("authors");
            if (authorsObj instanceof List) {
                authors = ((List<Object>) authorsObj).stream()
                        .map(Object::toString)
                        .collect(Collectors.toList());
            }

            String status = getStringValue(data, "status");
            LocalDateTime publishDate = parseDateTime(getStringValue(data, "publish_date"));
            LocalDateTime displayedDate = parseDateTime(getStringValue(data, "displayed_date"));
            String webUrl = getStringValue(data, "web_url");
            String thumbnailUrl = getStringValue(data, "thumbnail_url");
            String platform = getStringValue(data, "platform");
            String audience = getStringValue(data, "audience");

            // Extract content preview
            String contentPreview = null;
            Object contentObj = data.get("content");
            if (contentObj instanceof Map) {
                Map<String, Object> content = (Map<String, Object>) contentObj;
                Object freeWebContent = content.get("free_web_content");
                if (freeWebContent != null) {
                    String fullContent = freeWebContent.toString();
                    contentPreview = fullContent.length() > 200
                        ? fullContent.substring(0, 200) + "..."
                        : fullContent;
                }
            }

            // Extract content tags
            List<String> contentTags = new ArrayList<>();
            Object tagsObj = data.get("content_tags");
            if (tagsObj instanceof List) {
                contentTags = ((List<Object>) tagsObj).stream()
                        .map(Object::toString)
                        .collect(Collectors.toList());
            }

            // Extract stats if available
            PostStats stats = null;
            Object statsObj = data.get("stats");
            if (statsObj instanceof Map) {
                Map<String, Object> statsData = (Map<String, Object>) statsObj;
                long opens = getLongValue(statsData, "opens");
                long clicks = getLongValue(statsData, "clicks");
                long uniqueOpens = getLongValue(statsData, "unique_opens");
                long uniqueClicks = getLongValue(statsData, "unique_clicks");
                stats = new PostStats(opens, clicks, uniqueOpens, uniqueClicks);
            }

            return new Post(
                    id,
                    publicationId,
                    publicationName,
                    title,
                    authors,
                    status,
                    publishDate,
                    displayedDate,
                    webUrl,
                    thumbnailUrl,
                    contentPreview,
                    platform,
                    audience,
                    contentTags,
                    stats
            );
        } catch (Exception e) {
            logger.warn("Failed to convert API data to Post: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Parse datetime string from API
     */
    private LocalDateTime parseDateTime(String dateStr) {
        if (dateStr == null || dateStr.trim().isEmpty()) {
            return null;
        }

        try {
            // Try to parse as Unix timestamp (epoch seconds)
            try {
                long epochSeconds = Long.parseLong(dateStr);
                return LocalDateTime.ofEpochSecond(epochSeconds, 0, java.time.ZoneOffset.UTC);
            } catch (NumberFormatException e) {
                // Not a number, try ISO 8601 formats
            }

            // Beehiiv API may return ISO 8601 format
            DateTimeFormatter[] formatters = {
                DateTimeFormatter.ISO_DATE_TIME,
                DateTimeFormatter.ISO_LOCAL_DATE_TIME,
                DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSSSS'Z'"),
                DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss'Z'"),
                DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
            };

            for (DateTimeFormatter formatter : formatters) {
                try {
                    return LocalDateTime.parse(dateStr.replace("Z", ""), formatter);
                } catch (DateTimeParseException ignored) {
                    // Try next formatter
                }
            }

            logger.warn("Could not parse date: {}", dateStr);
            return null;
        } catch (Exception e) {
            logger.warn("Error parsing date '{}': {}", dateStr, e.getMessage());
            return null;
        }
    }

    /**
     * Get long value from map, handling various numeric types
     */
    private long getLongValue(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) return 0L;
        if (value instanceof Number) {
            return ((Number) value).longValue();
        }
        try {
            return Long.parseLong(value.toString());
        } catch (NumberFormatException e) {
            return 0L;
        }
    }

    /**
     * Check if post matches keyword search
     */
    private boolean matchesKeyword(Post post, String keyword) {
        String title = post.title() != null ? post.title().toLowerCase() : "";
        String contentPreview = post.contentPreview() != null ? post.contentPreview().toLowerCase() : "";
        String authors = post.getAuthorsFormatted().toLowerCase();

        return title.contains(keyword) || contentPreview.contains(keyword) || authors.contains(keyword);
    }

    /**
     * Safely extract string value from map, handling different data types
     */
    private String getStringValue(Map<String, Object> map, String key) {
        Object value = map.get(key);
        if (value == null) {
            return null;
        }
        return value.toString();
    }
}
