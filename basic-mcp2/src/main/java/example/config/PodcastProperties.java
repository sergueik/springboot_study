package example.config;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.NotNull;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.validation.annotation.Validated;

import java.time.Duration;
import java.util.Optional;

/**
 * Configuration properties for Transistor.fm podcast API integration
 */
@ConfigurationProperties(prefix = "dvaas.podcast")
@Validated
public record PodcastProperties(

        /**
         * Transistor.fm API key
         * Must be a valid, non-empty API key
         */
        @NotBlank(message = "Podcast API key must not be blank")
        String apiKey,

        /**
         * Application name to use for Transistor API requests
         * Default: "dvaas-podcast-mcp"
         */
        String applicationName,

        /**
         * Cache duration for podcast data
         * Must be at least 1 minute, default: 30 minutes
         */
        @NotNull(message = "Podcast cache duration must not be null")
        Duration cacheDuration,

        /**
         * Show ID for "Spring Office Hours" podcast
         */
        String springOfficeHoursShowId,

        /**
         * Show ID for "Fundamentals of Software Engineering" podcast
         */
        String fundamentalsShowId

) {

    /**
     * Create default PodcastProperties with sensible defaults and validation
     */
    public PodcastProperties {
        if (applicationName == null || applicationName.trim().isEmpty()) {
            applicationName = "dvaas-podcast-mcp";
        }

        if (cacheDuration == null) {
            cacheDuration = Duration.ofMinutes(30);
        }

        // Custom validation: cache duration must be at least 1 minute
        if (cacheDuration.toMinutes() < 1) {
            throw new IllegalArgumentException("Podcast cache duration must be at least 1 minute, got: " + cacheDuration);
        }

        // API key validation
        if (apiKey != null && (apiKey.length() < 10 || apiKey.length() > 100)) {
            throw new IllegalArgumentException("Podcast API key length seems invalid. Expected 10-100 characters, got: " + apiKey.length());
        }
    }

    /**
     * Get cache duration in minutes
     */
    public long getCacheDurationMinutes() {
        return cacheDuration.toMinutes();
    }

    /**
     * Check if podcast integration is properly configured
     */
    public boolean isEnabled() {
        return apiKey != null && !apiKey.trim().isEmpty();
    }

    /**
     * Resolve show identifier (name or ID) to show ID
     * Returns Optional.empty() if the identifier doesn't match any configured show
     */
    public Optional<String> getShowIdByName(String identifier) {
        if (identifier == null || identifier.trim().isEmpty()) {
            return Optional.empty();
        }

        String normalized = identifier.trim().toLowerCase();

        // Check for "Spring Office Hours" variations
        if (normalized.contains("spring office hours") ||
            normalized.contains("spring-office-hours") ||
            normalized.equals("soh")) {
            return Optional.ofNullable(springOfficeHoursShowId);
        }

        // Check for "Fundamentals of Software Engineering" variations
        if (normalized.contains("fundamentals") ||
            normalized.contains("fundamentals of software engineering") ||
            normalized.equals("fse")) {
            return Optional.ofNullable(fundamentalsShowId);
        }

        // If it doesn't match a known name, return empty (caller should try as ID)
        return Optional.empty();
    }

    /**
     * Check if the given identifier matches a configured show name
     */
    public boolean isKnownShowName(String identifier) {
        return getShowIdByName(identifier).isPresent();
    }
}
