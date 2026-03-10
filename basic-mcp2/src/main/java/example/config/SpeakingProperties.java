package example.config;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Pattern;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import java.time.Duration;

/**
 * Configuration properties for speaking content API integration
 */
@ConfigurationProperties(prefix = "dvaas.speaking")
@Validated
public record SpeakingProperties(

        /**
         * Speaking API endpoint URL
         * Must be a valid, non-empty URL starting with http:// or https://
         */
        @NotBlank(message = "Speaking API URL must not be blank")
        @Pattern(regexp = "^https?://.*", message = "Speaking API URL must start with http:// or https://")
        String apiUrl,

        /**
         * Cache duration for speaking data
         * Must be at least 1 minute, default: 30 minutes
         */
        @NotNull(message = "Speaking cache duration must not be null")
        Duration cacheDuration

) {

    /**
     * Create default SpeakingProperties with sensible defaults and validation
     */
    public SpeakingProperties {
        if (cacheDuration == null) {
            cacheDuration = Duration.ofMinutes(30);
        }

        // Custom validation: cache duration must be at least 1 minute
        if (cacheDuration.toMinutes() < 1) {
            throw new IllegalArgumentException("Speaking cache duration must be at least 1 minute, got: " + cacheDuration);
        }
    }

    /**
     * Get cache duration in minutes
     */
    public long getCacheDurationMinutes() {
        return cacheDuration.toMinutes();
    }

    /**
     * Check if speaking API is configured
     */
    public boolean isEnabled() {
        return apiUrl != null && !apiUrl.trim().isEmpty();
    }
}