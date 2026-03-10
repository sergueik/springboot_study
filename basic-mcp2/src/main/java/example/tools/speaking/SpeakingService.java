package example.tools.speaking;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;
import example.config.SpeakingProperties;
import example.tools.speaking.model.SpeakingEngagement;
import example.tools.speaking.model.SpeakingSearchResult;
import example.tools.speaking.model.SpeakingStats;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.time.temporal.ChronoUnit;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.stream.Collectors;

/**
 * Service for fetching and managing speaking engagement data
 */
@Service
@ConditionalOnProperty(name = "dvaas.speaking.api-url")
public class SpeakingService {

    private static final Logger logger = LoggerFactory.getLogger(SpeakingService.class);

    private final SpeakingProperties speakingProperties;
    private final HttpClient httpClient;
    private final ObjectMapper objectMapper;
    private final Map<String, Object> cache = new ConcurrentHashMap<>();
    private LocalDateTime lastCacheTime;

    public SpeakingService(SpeakingProperties speakingProperties) {
        this.speakingProperties = speakingProperties;
        this.httpClient = HttpClient.newHttpClient();
        this.objectMapper = new ObjectMapper();
        logger.info("Speaking service initialized with API URL: {}", speakingProperties.apiUrl());
        logger.info("Speaking cache duration: {} minutes", speakingProperties.getCacheDurationMinutes());
    }

    /**
     * Get all speaking engagements
     */
    public List<SpeakingEngagement> getAllEngagements() {
        return getCachedEngagements();
    }

    /**
     * Get latest speaking engagements
     */
    public List<SpeakingEngagement> getLatestEngagements(int maxResults) {
        List<SpeakingEngagement> allEngagements = getCachedEngagements();
        return allEngagements.stream()
                .sorted((e1, e2) -> {
                    LocalDateTime date1 = e1.startDate() != null ? e1.startDate() : LocalDateTime.MIN;
                    LocalDateTime date2 = e2.startDate() != null ? e2.startDate() : LocalDateTime.MIN;
                    return date2.compareTo(date1);
                })
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Get upcoming speaking engagements
     */
    public List<SpeakingEngagement> getUpcomingEngagements(int maxResults) {
        List<SpeakingEngagement> allEngagements = getCachedEngagements();
        LocalDateTime now = LocalDateTime.now();

        return allEngagements.stream()
                .filter(engagement -> engagement.startDate() != null && engagement.startDate().isAfter(now))
                .sorted(Comparator.comparing(SpeakingEngagement::startDate))
                .limit(Math.min(maxResults, 50))
                .toList();
    }

    /**
     * Search speaking engagements by keyword in title or description
     */
    public SpeakingSearchResult searchEngagementsByKeyword(String keyword, int maxResults) {
        if (keyword == null || keyword.trim().isEmpty()) {
            return SpeakingSearchResult.forKeyword(List.of(), keyword);
        }

        String searchTerm = keyword.toLowerCase().trim();
        List<SpeakingEngagement> allEngagements = getCachedEngagements();

        List<SpeakingEngagement> matchingEngagements = allEngagements.stream()
                .filter(engagement -> matchesKeyword(engagement, searchTerm))
                .sorted((e1, e2) -> {
                    LocalDateTime date1 = e1.startDate() != null ? e1.startDate() : LocalDateTime.MIN;
                    LocalDateTime date2 = e2.startDate() != null ? e2.startDate() : LocalDateTime.MIN;
                    return date2.compareTo(date1);
                })
                .limit(Math.min(maxResults, 50))
                .toList();

        return SpeakingSearchResult.forKeyword(matchingEngagements, keyword);
    }

    /**
     * Get speaking engagements within a specific date range
     */
    public SpeakingSearchResult getEngagementsByDateRange(LocalDateTime startDate, LocalDateTime endDate, int maxResults) {
        List<SpeakingEngagement> allEngagements = getCachedEngagements();

        String dateRangeDesc = String.format("%s to %s",
            startDate.toLocalDate(), endDate.toLocalDate());

        List<SpeakingEngagement> matchingEngagements = allEngagements.stream()
                .filter(engagement -> isWithinDateRange(engagement.startDate(), startDate, endDate))
                .sorted((e1, e2) -> {
                    LocalDateTime date1 = e1.startDate() != null ? e1.startDate() : LocalDateTime.MIN;
                    LocalDateTime date2 = e2.startDate() != null ? e2.startDate() : LocalDateTime.MIN;
                    return date2.compareTo(date1);
                })
                .limit(Math.min(maxResults, 50))
                .toList();

        return SpeakingSearchResult.forDateRange(matchingEngagements, dateRangeDesc);
    }

    /**
     * Get speaking engagements from a specific year
     */
    public SpeakingSearchResult getEngagementsByYear(int year, int maxResults) {
        LocalDateTime startOfYear = LocalDateTime.of(year, 1, 1, 0, 0);
        LocalDateTime endOfYear = LocalDateTime.of(year, 12, 31, 23, 59);

        SpeakingSearchResult result = getEngagementsByDateRange(startOfYear, endOfYear, maxResults);
        return SpeakingSearchResult.forDateRange(result.engagements(), String.valueOf(year));
    }

    /**
     * Get overall speaking statistics
     */
    public SpeakingStats getSpeakingStats() {
        List<SpeakingEngagement> allEngagements = getCachedEngagements();

        if (allEngagements.isEmpty()) {
            return new SpeakingStats(0, 0, 0, null, null, null, null, Map.of(), Map.of(), 0.0);
        }

        LocalDateTime now = LocalDateTime.now();

        // Count upcoming and past events
        long upcomingCount = allEngagements.stream()
                .filter(e -> e.startDate() != null && e.startDate().isAfter(now))
                .count();

        long pastCount = allEngagements.stream()
                .filter(e -> {
                    LocalDateTime eventEnd = e.endDate() != null ? e.endDate() : e.startDate();
                    return eventEnd != null && eventEnd.isBefore(now);
                })
                .count();

        // Find date range
        List<SpeakingEngagement> sortedEngagements = allEngagements.stream()
                .filter(e -> e.startDate() != null)
                .sorted(Comparator.comparing(SpeakingEngagement::startDate))
                .toList();

        LocalDateTime firstEventDate = sortedEngagements.isEmpty() ? null : sortedEngagements.get(0).startDate();
        LocalDateTime nextEventDate = allEngagements.stream()
                .filter(e -> e.startDate() != null && e.startDate().isAfter(now))
                .min(Comparator.comparing(SpeakingEngagement::startDate))
                .map(SpeakingEngagement::startDate)
                .orElse(null);

        // Calculate location and event type statistics
        Map<String, Integer> locationCounts = allEngagements.stream()
                .filter(e -> e.location() != null && !e.location().trim().isEmpty())
                .collect(Collectors.groupingBy(
                    e -> e.location().trim(),
                    Collectors.summingInt(e -> 1)
                ));

        Map<String, Integer> eventTypeCounts = allEngagements.stream()
                .collect(Collectors.groupingBy(
                    SpeakingEngagement::getEventType,
                    Collectors.summingInt(e -> 1)
                ));

        String mostCommonLocation = locationCounts.entrySet().stream()
                .max(Map.Entry.comparingByValue())
                .map(Map.Entry::getKey)
                .orElse("Virtual");

        String mostCommonEventType = eventTypeCounts.entrySet().stream()
                .max(Map.Entry.comparingByValue())
                .map(Map.Entry::getKey)
                .orElse("Conference");

        // Calculate average events per month
        double averageEventsPerMonth = 0.0;
        if (firstEventDate != null) {
            long monthsBetween = ChronoUnit.MONTHS.between(firstEventDate, now) + 1;
            averageEventsPerMonth = monthsBetween > 0 ? (double) allEngagements.size() / monthsBetween : 0;
        }

        return new SpeakingStats(
            allEngagements.size(),
            (int) upcomingCount,
            (int) pastCount,
            firstEventDate,
            nextEventDate,
            mostCommonLocation,
            mostCommonEventType,
            locationCounts,
            eventTypeCounts,
            averageEventsPerMonth
        );
    }

    /**
     * Get cached engagements, refreshing cache if needed
     */
    @SuppressWarnings("unchecked")
    private List<SpeakingEngagement> getCachedEngagements() {
        if (needsCacheRefresh()) {
            try {
                List<SpeakingEngagement> engagements = fetchEngagementsFromApi();
                cache.put("engagements", engagements);
                lastCacheTime = LocalDateTime.now();
                logger.info("Speaking API cache refreshed with {} engagements", engagements.size());
                return engagements;
            } catch (Exception e) {
                logger.error("Failed to refresh speaking API cache", e);
                // Return cached engagements if available, otherwise empty list
                return (List<SpeakingEngagement>) cache.getOrDefault("engagements", List.of());
            }
        }

        return (List<SpeakingEngagement>) cache.getOrDefault("engagements", List.of());
    }

    /**
     * Check if cache needs to be refreshed
     */
    private boolean needsCacheRefresh() {
        return lastCacheTime == null ||
               ChronoUnit.MINUTES.between(lastCacheTime, LocalDateTime.now()) >= speakingProperties.getCacheDurationMinutes() ||
               !cache.containsKey("engagements");
    }

    /**
     * Fetch speaking engagements from API
     */
    private List<SpeakingEngagement> fetchEngagementsFromApi() throws IOException, InterruptedException {
        logger.info("Fetching speaking data from: {}", speakingProperties.apiUrl());

        HttpRequest request = HttpRequest.newBuilder()
                .uri(URI.create(speakingProperties.apiUrl()))
                .header("Accept", "application/json")
                .GET()
                .build();

        HttpResponse<String> response = httpClient.send(request, HttpResponse.BodyHandlers.ofString());

        if (response.statusCode() != 200) {
            throw new IOException("HTTP " + response.statusCode() + ": " + response.body());
        }

        // Parse JSON response
        List<Map<String, Object>> apiData = objectMapper.readValue(
            response.body(),
            new TypeReference<List<Map<String, Object>>>() {}
        );

        List<SpeakingEngagement> engagements = new ArrayList<>();

        for (Map<String, Object> item : apiData) {
            SpeakingEngagement engagement = convertApiDataToEngagement(item);
            if (engagement != null) {
                engagements.add(engagement);
            }
        }

        logger.info("Successfully parsed {} speaking engagements from API", engagements.size());
        return engagements;
    }

    /**
     * Convert API data map to SpeakingEngagement
     */
    private SpeakingEngagement convertApiDataToEngagement(Map<String, Object> data) {
        try {
            String title = (String) data.get("title");
            String url = (String) data.get("url");
            String name = (String) data.get("name");
            String location = (String) data.get("location");
            String description = (String) data.get("description");

            LocalDateTime startDate = parseDateTime((String) data.get("startDate"));
            LocalDateTime endDate = parseDateTime((String) data.get("endDate"));

            return new SpeakingEngagement(title, url, name, startDate, endDate, location, description);
        } catch (Exception e) {
            logger.warn("Failed to convert API data to SpeakingEngagement: {}", e.getMessage());
            return null;
        }
    }

    /**
     * Parse date time string from API
     */
    private LocalDateTime parseDateTime(String dateStr) {
        if (dateStr == null || dateStr.trim().isEmpty()) {
            return null;
        }

        try {
            // Try various date formats
            DateTimeFormatter[] formatters = {
                DateTimeFormatter.ISO_LOCAL_DATE_TIME,
                DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss"),
                DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"),
                DateTimeFormatter.ofPattern("yyyy-MM-dd"),
                DateTimeFormatter.ofPattern("MM/dd/yyyy"),
                DateTimeFormatter.ofPattern("dd/MM/yyyy")
            };

            for (DateTimeFormatter formatter : formatters) {
                try {
                    if (formatter.toString().contains("HH")) {
                        return LocalDateTime.parse(dateStr, formatter);
                    } else {
                        return LocalDateTime.parse(dateStr + "T00:00:00");
                    }
                } catch (DateTimeParseException ignored) {
                    // Try next formatter
                }
            }

            logger.warn("Could not parse date: {}", dateStr);
            return null;
        } catch (Exception e) {
            logger.warn("Error parsing date '{}': {}", dateStr, e.getMessage());
            return null;
        }
    }

    /**
     * Check if engagement matches keyword search
     */
    private boolean matchesKeyword(SpeakingEngagement engagement, String keyword) {
        String title = engagement.title() != null ? engagement.title().toLowerCase() : "";
        String description = engagement.description() != null ? engagement.description().toLowerCase() : "";
        String name = engagement.name() != null ? engagement.name().toLowerCase() : "";
        String location = engagement.location() != null ? engagement.location().toLowerCase() : "";

        return title.contains(keyword) || description.contains(keyword) ||
               name.contains(keyword) || location.contains(keyword);
    }

    /**
     * Check if date is within range (inclusive)
     */
    private boolean isWithinDateRange(LocalDateTime date, LocalDateTime start, LocalDateTime end) {
        if (date == null) return false;
        return !date.isBefore(start) && !date.isAfter(end);
    }
}