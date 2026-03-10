package example.config;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import java.time.Duration;

/**
 * Configuration properties for blog RSS feed integration
 */
@ConfigurationProperties(prefix = "dvaas.blog")
@Validated
public record BlogProperties(

        /**
         * RSS feed URL for the blog
         * Must be a valid, non-empty URL starting with http:// or https://
         */
        @NotBlank(message = "Blog RSS URL must not be blank")
        @Pattern(regexp = "^https?://.*", message = "Blog RSS URL must start with http:// or https://")
        String rssUrl,

        /**
         * Cache duration for RSS feed data
         * Must be at least 1 minute, default: 30 minutes
         */
        @NotNull(message = "Blog cache duration must not be null")
        Duration cacheDuration

) {

    /**
     * Create default BlogProperties with sensible defaults and validation
     */
    public BlogProperties {
        if (cacheDuration == null) {
            cacheDuration = Duration.ofMinutes(30);
        }

        // Custom validation: cache duration must be at least 1 minute
        if (cacheDuration.toMinutes() < 1) {
            throw new IllegalArgumentException("Blog cache duration must be at least 1 minute, got: " + cacheDuration);
        }
    }

    /**
     * Get cache duration in minutes
     */
    public long getCacheDurationMinutes() {
        return cacheDuration.toMinutes();
    }

    /**
     * Check if RSS URL is configured
     */
    public boolean isEnabled() {
        return rssUrl != null && !rssUrl.trim().isEmpty();
    }
}