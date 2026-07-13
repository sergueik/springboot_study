package example.service;

import java.time.Duration;
import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.stereotype.Service;

import example.config.AppProperties;
import example.filter.IpAddressMatcher;

@Service
public class IpAllowlistService {
    private final AppProperties appProperties;
    private final Map<String, Instant> dynamicIps = new ConcurrentHashMap<>();

    public IpAllowlistService(AppProperties appProperties) {
        this.appProperties = appProperties;
    }

    public boolean isAllowed(String remoteAddress) {
        boolean staticMatch = appProperties.getSecurity().getAllowedIps().stream().anyMatch(pattern -> new IpAddressMatcher(pattern).matches(remoteAddress));

        if (staticMatch) {
            return true;
        }
        cleanupExpired();
        return dynamicIps.containsKey(remoteAddress);
    }

    public Instant allow(String remoteAddress) {
        long ttlMinutes = appProperties.getSecurity().getDynamicIpTtlMinutes();

        Instant expiresAt = Instant.now().plus(Duration.ofMinutes(ttlMinutes));
        dynamicIps.put(remoteAddress, expiresAt);
        return expiresAt;
    }

    private void cleanupExpired() {
        Instant now = Instant.now();
        dynamicIps.entrySet().removeIf(entry -> entry.getValue().isBefore(now));
    }
}