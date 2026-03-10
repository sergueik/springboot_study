package example.tools.newsletter.model;

import java.util.List;

/**
 * Represents search results for newsletter posts
 */
public record PostSearchResult(
        List<Post> posts,
        int totalResults,
        String searchCriteria,
        String publicationFilter
) {

    /**
     * Create search result for keyword search
     */
    public static PostSearchResult forKeyword(List<Post> posts, String keyword, String publication) {
        return new PostSearchResult(
                posts,
                posts.size(),
                "keyword: " + keyword,
                publication
        );
    }

    /**
     * Create search result for status filter
     */
    public static PostSearchResult forStatus(List<Post> posts, String status, String publication) {
        return new PostSearchResult(
                posts,
                posts.size(),
                "status: " + status,
                publication
        );
    }

    /**
     * Create search result for latest posts
     */
    public static PostSearchResult forLatest(List<Post> posts, String publication) {
        return new PostSearchResult(
                posts,
                posts.size(),
                "latest posts",
                publication
        );
    }

    /**
     * Check if search returned any results
     */
    public boolean hasResults() {
        return posts != null && !posts.isEmpty();
    }

    /**
     * Get result count
     */
    public int getResultCount() {
        return posts != null ? posts.size() : 0;
    }
}
