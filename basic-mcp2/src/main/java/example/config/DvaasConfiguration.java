package example.config;

import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.context.annotation.Configuration;

/**
 * Main configuration class for dvaas application
 * Enables configuration properties for all features
 */
@Configuration
@EnableConfigurationProperties({
    BlogProperties.class,
    YouTubeProperties.class,
    SpeakingProperties.class,
    NewsletterProperties.class,
    PodcastProperties.class
})
public class DvaasConfiguration {
}