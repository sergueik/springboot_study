package com.bookportal.api.configs;

import com.bookportal.api.service.Top20Service;
import lombok.RequiredArgsConstructor;
import org.springframework.cache.CacheManager;
import org.springframework.cache.annotation.EnableCaching;
import org.springframework.cache.concurrent.ConcurrentMapCacheManager;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

@Configuration
@EnableCaching
@EnableScheduling
@RequiredArgsConstructor
public class CachingConfig {
    private final Top20Service top20Service;
    public static final long ONE_HOUR = 3600000L;
    private static final long TWELVE_HOUR = 12 * 3600000L;

    @Bean
    public CacheManager cacheManager() {
        return new ConcurrentMapCacheManager("homePage");
    }

    @Scheduled(fixedRate = TWELVE_HOUR)
    public void updateTop20() {
        top20Service.updateTop20();
    }
}
