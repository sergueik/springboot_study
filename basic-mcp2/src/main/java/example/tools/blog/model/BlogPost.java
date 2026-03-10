package example.tools.blog.model;

import java.time.LocalDateTime;
import java.util.List;

/**
 * Represents a blog post from RSS feed for MCP tool responses
 */
public record BlogPost(
        String title,
        String link,
        String guid,
        String description,
        LocalDateTime publishedAt,
        String author,
        List<String> tags,
        String youtubeVideoUrl
) {

    /**
     * Create a basic BlogPost with essential information
     */
    public static BlogPost basic(String title, String link, String guid, LocalDateTime publishedAt) {
        return new BlogPost(title, link, guid, null, publishedAt, null, List.of(), null);
    }

    /**
     * Get the full blog post URL (convert relative to absolute)
     */
    public String getFullUrl() {
        if (link != null && link.startsWith("http")) {
            return link;
        }
        return "https://www.danvega.dev" + (link != null ? link : "");
    }

    /**
     * Get a shortened description for display
     */
    public String getShortDescription() {
        if (description == null || description.isEmpty()) {
            return "";
        }
        return description.length() > 200 ? description.substring(0, 200) + "..." : description;
    }

    /**
     * Check if this post has an associated YouTube video
     */
    public boolean hasYouTubeVideo() {
        return youtubeVideoUrl != null && !youtubeVideoUrl.isEmpty();
    }

    /**
     * Extract potential tags from title and description
     */
    public List<String> extractPotentialTags() {
        if (tags != null && !tags.isEmpty()) {
            return tags;
        }

        // Extract common tech terms from title and description as potential tags
        String content = (title + " " + (description != null ? description : "")).toLowerCase();
        return List.of("spring", "java", "boot", "ai", "graphql", "react", "vue", "docker", "kubernetes")
                .stream()
                .filter(tag -> content.contains(tag))
                .toList();
    }
}