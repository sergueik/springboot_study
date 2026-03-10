package example.config;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

/**
 * Configuration properties for YouTube Data API integration
 */
@ConfigurationProperties(prefix = "dvaas.youtube")
@Validated
public record YouTubeProperties(

        /**
         * YouTube Data API key
         * Must be a valid, non-empty API key
         */
        @NotBlank(message = "YouTube API key must not be blank")
        String apiKey,

        /**
         * YouTube channel ID to fetch data from
         * Must be a valid, non-empty channel ID (typically starts with UC)
         */
        @NotBlank(message = "YouTube channel ID must not be blank")
        @Pattern(regexp = "^(UC|UU|HC)[a-zA-Z0-9_-]{22}$",
                message = "YouTube channel ID must be a valid format (24 characters starting with UC, UU, or HC)")
        String channelId,

        /**
         * Application name to use for YouTube API requests
         * Must be non-blank, default: "dvaas-youtube-mcp"
         */
        @NotBlank(message = "YouTube application name must not be blank")
        String applicationName

) {

    /**
     * Create default YouTubeProperties with sensible defaults and validation
     */
    public YouTubeProperties {
        if (applicationName == null || applicationName.trim().isEmpty()) {
            applicationName = "dvaas-youtube-mcp";
        }

        // Additional validation for API key format (should be reasonable length)
        if (apiKey != null && (apiKey.length() < 10 || apiKey.length() > 100)) {
            throw new IllegalArgumentException("YouTube API key length seems invalid. Expected 10-100 characters, got: " + apiKey.length());
        }
    }

    /**
     * Check if YouTube integration is properly configured
     */
    public boolean isEnabled() {
        return apiKey != null && !apiKey.trim().isEmpty() &&
               channelId != null && !channelId.trim().isEmpty();
    }
}