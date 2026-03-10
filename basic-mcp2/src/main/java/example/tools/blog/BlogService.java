package example.tools.blog;

import com.rometools.rome.feed.synd.SyndEntry;
import com.rometools.rome.feed.synd.SyndFeed;
import com.rometools.rome.io.SyndFeedInput;
import com.rometools.rome.io.XmlReader;
import example.config.BlogProperties;
import example.tools.blog.model.BlogPost;
import example.tools.blog.model.BlogStats;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.net.URL;
import java.time.LocalDateTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Service for parsing and managing blog RSS feed data
 */
@Service
@ConditionalOnProperty(name = "dvaas.blog.rss-url")
public class BlogService {

    private static final Logger logger = LoggerFactory.getLogger(BlogService.class);

    private final BlogProperties blogProperties;
    private final Map<String, Object> cache = new ConcurrentHashMap<>();
    private LocalDateTime lastCacheTime;

    public BlogService(BlogProperties blogProperties) {
        this.blogProperties = blogProperties;
        logger.info("Blog service initialized with RSS URL: {}", blogProperties.rssUrl());
        logger.info("Blog cache duration: {} minutes", blogProperties.getCacheDurationMinutes());
    }

    /**
     * Get all blog posts from the RSS feed
     */
    public List<BlogPost> getAllPosts() {
        return getCachedPosts();
    }

    /**
     * Get latest blog posts
     */
    public List<BlogPost> getLatestPosts(int maxResults) {
        List<BlogPost> allPosts = getCachedPosts();
        return allPosts.stream()
                .sorted((p1, p2) -> p2.publishedAt().compareTo(p1.publishedAt()))
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Search blog posts by keyword in title and description
     */
    public List<BlogPost> searchPostsByKeyword(String keyword, int maxResults) {
        if (keyword == null || keyword.trim().isEmpty()) {
            return List.of();
        }

        String searchTerm = keyword.toLowerCase().trim();
        List<BlogPost> allPosts = getCachedPosts();

        return allPosts.stream()
                .filter(post -> matchesKeyword(post, searchTerm))
                .sorted((p1, p2) -> p2.publishedAt().compareTo(p1.publishedAt()))
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Get blog posts within a specific date range
     */
    public List<BlogPost> getPostsByDateRange(LocalDateTime startDate, LocalDateTime endDate, int maxResults) {
        List<BlogPost> allPosts = getCachedPosts();

        return allPosts.stream()
                .filter(post -> isWithinDateRange(post.publishedAt(), startDate, endDate))
                .sorted((p1, p2) -> p2.publishedAt().compareTo(p1.publishedAt()))
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Get blog posts from a specific year
     */
    public List<BlogPost> getPostsByYear(int year, int maxResults) {
        LocalDateTime startOfYear = LocalDateTime.of(year, 1, 1, 0, 0);
        LocalDateTime endOfYear = LocalDateTime.of(year, 12, 31, 23, 59);

        return getPostsByDateRange(startOfYear, endOfYear, maxResults);
    }

    /**
     * Get overall blog statistics
     */
    public BlogStats getBlogStats() {
        List<BlogPost> allPosts = getCachedPosts();

        if (allPosts.isEmpty()) {
            return new BlogStats(0, null, null, 0, 0, 0.0, 0, null);
        }

        // Sort posts by date for calculations
        List<BlogPost> sortedPosts = allPosts.stream()
                .sorted(Comparator.comparing(BlogPost::publishedAt))
                .toList();

        LocalDateTime firstPost = sortedPosts.get(0).publishedAt();
        LocalDateTime latestPost = sortedPosts.get(sortedPosts.size() - 1).publishedAt();
        LocalDateTime now = LocalDateTime.now();

        // Calculate posts this year and month
        int currentYear = now.getYear();
        int currentMonth = now.getMonthValue();

        int postsThisYear = (int) allPosts.stream()
                .filter(post -> post.publishedAt().getYear() == currentYear)
                .count();

        int postsThisMonth = (int) allPosts.stream()
                .filter(post -> post.publishedAt().getYear() == currentYear &&
                               post.publishedAt().getMonthValue() == currentMonth)
                .count();

        // Calculate average posts per month
        long monthsBetween = ChronoUnit.MONTHS.between(firstPost, latestPost) + 1;
        double averagePostsPerMonth = monthsBetween > 0 ? (double) allPosts.size() / monthsBetween : 0;

        // Count posts with YouTube videos
        int postsWithVideos = (int) allPosts.stream()
                .filter(BlogPost::hasYouTubeVideo)
                .count();

        // Find most common tag
        String mostCommonTag = findMostCommonTag(allPosts);

        return new BlogStats(
            allPosts.size(),
            firstPost,
            latestPost,
            postsThisYear,
            postsThisMonth,
            averagePostsPerMonth,
            postsWithVideos,
            mostCommonTag
        );
    }

    /**
     * Get cached posts, refreshing cache if needed
     */
    @SuppressWarnings("unchecked")
    private List<BlogPost> getCachedPosts() {
        if (needsCacheRefresh()) {
            try {
                List<BlogPost> posts = fetchPostsFromRss();
                cache.put("posts", posts);
                lastCacheTime = LocalDateTime.now();
                logger.info("RSS feed cache refreshed with {} posts", posts.size());
                return posts;
            } catch (Exception e) {
                logger.error("Failed to refresh RSS feed cache", e);
                // Return cached posts if available, otherwise empty list
                return (List<BlogPost>) cache.getOrDefault("posts", List.of());
            }
        }

        return (List<BlogPost>) cache.getOrDefault("posts", List.of());
    }

    /**
     * Check if cache needs to be refreshed
     */
    private boolean needsCacheRefresh() {
        return lastCacheTime == null ||
               ChronoUnit.MINUTES.between(lastCacheTime, LocalDateTime.now()) >= blogProperties.getCacheDurationMinutes() ||
               !cache.containsKey("posts");
    }

    /**
     * Fetch blog posts from RSS feed
     */
    private List<BlogPost> fetchPostsFromRss() throws Exception {
        logger.info("Fetching RSS feed from: {}", blogProperties.rssUrl());

        SyndFeedInput input = new SyndFeedInput();
        SyndFeed feed = input.build(new XmlReader(new URL(blogProperties.rssUrl())));

        List<BlogPost> posts = new ArrayList<>();

        for (SyndEntry entry : feed.getEntries()) {
            BlogPost post = convertEntryToBlogPost(entry);
            if (post != null) {
                posts.add(post);
            }
        }

        logger.info("Successfully parsed {} posts from RSS feed", posts.size());
        return posts;
    }

    /**
     * Convert SyndEntry to BlogPost
     */
    private BlogPost convertEntryToBlogPost(SyndEntry entry) {
        try {
            String title = entry.getTitle();
            String link = entry.getLink();
            String guid = entry.getUri();
            String description = entry.getDescription() != null ? entry.getDescription().getValue() : "";
            String author = entry.getAuthor();

            LocalDateTime publishedAt = entry.getPublishedDate() != null ?
                entry.getPublishedDate().toInstant().atZone(ZoneId.systemDefault()).toLocalDateTime() :
                LocalDateTime.now();

            // Extract YouTube video URL if present in description or content
            String youtubeUrl = extractYouTubeUrl(description);

            // Extract potential tags from title and description
            List<String> tags = extractTagsFromContent(title, description);

            return new BlogPost(title, link, guid, description, publishedAt, author, tags, youtubeUrl);
        } catch (Exception e) {
            logger.warn("Failed to convert RSS entry to BlogPost: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Check if post matches keyword search
     */
    private boolean matchesKeyword(BlogPost post, String keyword) {
        String title = post.title() != null ? post.title().toLowerCase() : "";
        String description = post.description() != null ? post.description().toLowerCase() : "";

        return title.contains(keyword) || description.contains(keyword);
    }

    /**
     * Check if date is within range (inclusive)
     */
    private boolean isWithinDateRange(LocalDateTime date, LocalDateTime start, LocalDateTime end) {
        return !date.isBefore(start) && !date.isAfter(end);
    }

    /**
     * Extract YouTube video URL from content
     */
    private String extractYouTubeUrl(String content) {
        if (content == null) return null;

        // Look for YouTube URLs in the content
        String[] patterns = {
            "https://www.youtube.com/watch\\?v=([a-zA-Z0-9_-]+)",
            "https://youtu.be/([a-zA-Z0-9_-]+)",
            "youtube.com/embed/([a-zA-Z0-9_-]+)"
        };

        for (String pattern : patterns) {
            java.util.regex.Pattern p = java.util.regex.Pattern.compile(pattern);
            java.util.regex.Matcher m = p.matcher(content);
            if (m.find()) {
                return m.group(0);
            }
        }

        return null;
    }

    /**
     * Extract potential tags from title and description
     */
    private List<String> extractTagsFromContent(String title, String description) {
        String content = (title + " " + (description != null ? description : "")).toLowerCase();

        List<String> commonTechTerms = List.of(
            "spring", "java", "boot", "ai", "graphql", "react", "vue", "docker",
            "kubernetes", "microservices", "rest", "api", "jwt", "security",
            "testing", "junit", "maven", "gradle", "git", "devops", "cloud",
            "aws", "azure", "gcp", "database", "sql", "nosql", "mongodb",
            "redis", "elasticsearch", "kafka", "rabbitmq", "jpa", "hibernate"
        );

        return commonTechTerms.stream()
                .filter(tag -> content.contains(tag))
                .collect(Collectors.toList());
    }

    /**
     * Find the most common tag across all posts
     */
    private String findMostCommonTag(List<BlogPost> posts) {
        Map<String, Long> tagCounts = posts.stream()
                .flatMap(post -> post.extractPotentialTags().stream())
                .collect(Collectors.groupingBy(tag -> tag, Collectors.counting()));

        return tagCounts.entrySet().stream()
                .max(Map.Entry.comparingByValue())
                .map(Map.Entry::getKey)
                .orElse("spring"); // Default to "spring" as it's likely to be common in Dan's blog
    }
}