package com.yxrkt.agent.error;

import com.yxrkt.agent.bus.EventBus;
import org.openqa.selenium.*;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Supplier;

public class ErrorHandler {
    private static final Map<String, Integer> retryCounters = new ConcurrentHashMap<>();
    private static final Map<String, Long> lastRetryTime = new ConcurrentHashMap<>();
    private static final Set<Class<? extends Throwable>> retryableExceptions = new HashSet<>();
    
    // Configuration
    private static int maxRetries = 3;
    private static long backoffMs = 1000;
    private static boolean enableRetry = true;
    
    static {
        // Define retryable exceptions
        retryableExceptions.add(StaleElementReferenceException.class);
        retryableExceptions.add(ElementNotInteractableException.class);
        retryableExceptions.add(ElementClickInterceptedException.class);
        retryableExceptions.add(TimeoutException.class);
        retryableExceptions.add(WebDriverException.class);
    }
    
    public static void setMaxRetries(int retries) {
        maxRetries = retries;
    }
    
    public static void setBackoffMs(long ms) {
        backoffMs = ms;
    }
    
    public static void setEnableRetry(boolean enable) {
        enableRetry = enable;
    }
    
    public static void addRetryableException(Class<? extends Throwable> exceptionClass) {
        retryableExceptions.add(exceptionClass);
    }
    
    public static <T> T executeWithRetry(String operationId, Supplier<T> operation, Object driver) {
        String key = operationId + "_" + Thread.currentThread().getId();
        int attempts = 0;
        Throwable lastException = null;
        
        while (attempts <= maxRetries) {
            try {
                if (attempts > 0) {
                    // Log retry attempt
                    final int finalAttempts = attempts;
                    final Throwable finalLastException = lastException;
                    EventBus.bus().cdp("error.retry.attempt", driver, j -> {
                        j.add("operationId", operationId);
                        j.add("attempt", finalAttempts);
                        j.add("maxRetries", maxRetries);
                        j.add("backoffMs", backoffMs);
                        j.add("lastError", finalLastException != null ? finalLastException.getClass().getSimpleName() : "unknown");
                        return null;
                    });
                    
                    // Apply backoff
                    Thread.sleep(backoffMs * attempts);
                }
                
                T result = operation.get();
                
                // Log successful recovery if this was a retry
                if (attempts > 0) {
                    final int finalAttempts = attempts;
                    final Throwable finalLastException = lastException;
                    EventBus.bus().cdp("error.recovery.success", driver, j -> {
                        j.add("operationId", operationId);
                        j.add("totalAttempts", finalAttempts + 1);
                        j.add("recoveredFrom", finalLastException != null ? finalLastException.getClass().getSimpleName() : "unknown");
                        return null;
                    });
                }
                
                // Clear retry counter on success
                retryCounters.remove(key);
                lastRetryTime.remove(key);
                
                return result;
                
            } catch (Throwable e) {
                lastException = e;
                attempts++;
                
                // Categorize the error
                String errorCategory = categorizeError(e);
                boolean isRetryable = isRetryableException(e) && enableRetry;
                
                // Log error details
                final int finalAttempts = attempts;
                final String finalErrorCategory = errorCategory;
                final boolean finalIsRetryable = isRetryable;
                EventBus.bus().cdp("error.occurred", driver, j -> {
                    j.add("operationId", operationId);
                    j.add("attempt", finalAttempts);
                    j.add("errorType", e.getClass().getSimpleName());
                    j.add("errorMessage", e.getMessage());
                    j.add("errorCategory", finalErrorCategory);
                    j.add("isRetryable", finalIsRetryable);
                    j.add("stackTrace", getStackTraceString(e));
                    return null;
                });
                
                // Update retry tracking
                retryCounters.put(key, attempts);
                lastRetryTime.put(key, System.currentTimeMillis());
                
                // Check if we should retry
                if (!isRetryable || attempts > maxRetries) {
                    // Log final failure
                    final int finalTotalAttempts = attempts;
                    final String finalErrorCategory2 = errorCategory;
                    EventBus.bus().cdp("error.final.failure", driver, j -> {
                        j.add("operationId", operationId);
                        j.add("totalAttempts", finalTotalAttempts);
                        j.add("finalError", e.getClass().getSimpleName());
                        j.add("finalErrorMessage", e.getMessage());
                        j.add("errorCategory", finalErrorCategory2);
                        j.add("retryExhausted", finalTotalAttempts > maxRetries);
                        return null;
                    });
                    
                    // Clean up tracking
                    retryCounters.remove(key);
                    lastRetryTime.remove(key);
                    
                    // Re-throw the exception
                    if (e instanceof RuntimeException) {
                        throw (RuntimeException) e;
                    } else {
                        throw new RuntimeException(e);
                    }
                }
            }
        }
        
        // This should never be reached, but just in case
        throw new RuntimeException("Unexpected error in retry logic", lastException);
    }
    
    private static boolean isRetryableException(Throwable e) {
        for (Class<? extends Throwable> retryableClass : retryableExceptions) {
            if (retryableClass.isAssignableFrom(e.getClass())) {
                return true;
            }
        }
        return false;
    }
    
    private static String categorizeError(Throwable e) {
        if (e instanceof org.openqa.selenium.NoSuchElementException) {
            return "ELEMENT_NOT_FOUND";
        } else if (e instanceof StaleElementReferenceException) {
            return "STALE_ELEMENT";
        } else if (e instanceof ElementNotInteractableException) {
            return "ELEMENT_NOT_INTERACTABLE";
        } else if (e instanceof ElementClickInterceptedException) {
            return "CLICK_INTERCEPTED";
        } else if (e instanceof TimeoutException) {
            return "TIMEOUT";
        } else if (e instanceof InvalidElementStateException) {
            return "INVALID_ELEMENT_STATE";
        } else if (e instanceof WebDriverException) {
            return "WEBDRIVER_ERROR";
        } else if (e instanceof InterruptedException) {
            return "INTERRUPTED";
        } else {
            return "UNKNOWN";
        }
    }
    
    private static String getStackTraceString(Throwable e) {
        StringBuilder sb = new StringBuilder();
        StackTraceElement[] elements = e.getStackTrace();
        
        // Limit stack trace to first 5 elements to avoid excessive data
        int limit = Math.min(5, elements.length);
        for (int i = 0; i < limit; i++) {
            sb.append(elements[i].toString());
            if (i < limit - 1) {
                sb.append("\n");
            }
        }
        
        if (elements.length > limit) {
            sb.append("\n... ").append(elements.length - limit).append(" more");
        }
        
        return sb.toString();
    }
    
    public static Map<String, Object> getRetryStatistics() {
        Map<String, Object> stats = new HashMap<>();
        stats.put("activeRetries", retryCounters.size());
        stats.put("retryCounters", new HashMap<>(retryCounters));
        stats.put("lastRetryTimes", new HashMap<>(lastRetryTime));
        stats.put("maxRetries", maxRetries);
        stats.put("backoffMs", backoffMs);
        stats.put("enableRetry", enableRetry);
        stats.put("retryableExceptions", retryableExceptions.stream()
            .map(Class::getSimpleName)
            .toArray(String[]::new));
        return stats;
    }
    
    public static String getRetryStatisticsAsJson() {
        Map<String, Object> stats = getRetryStatistics();
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        boolean first = true;
        
        for (Map.Entry<String, Object> entry : stats.entrySet()) {
            if (!first) {
                sb.append(",");
            }
            first = false;
            sb.append("\"").append(entry.getKey()).append("\":");
            
            Object value = entry.getValue();
            if (value == null) {
                sb.append("null");
            } else if (value instanceof String) {
                sb.append("\"").append(value.toString().replace("\"", "\\\"")).append("\"");
            } else if (value instanceof Number || value instanceof Boolean) {
                sb.append(value.toString());
            } else if (value instanceof Map) {
                sb.append(mapToJsonString((Map<?, ?>) value));
            } else if (value instanceof String[]) {
                sb.append("[");
                String[] arr = (String[]) value;
                for (int i = 0; i < arr.length; i++) {
                    if (i > 0) sb.append(",");
                    sb.append("\"").append(arr[i]).append("\"");
                }
                sb.append("]");
            } else {
                sb.append("\"").append(value.toString().replace("\"", "\\\"")).append("\"");
            }
        }
        sb.append("}");
        return sb.toString();
    }
    
    private static String mapToJsonString(Map<?, ?> map) {
        if (map == null || map.isEmpty()) {
            return "{}";
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        boolean first = true;
        for (Map.Entry<?, ?> entry : map.entrySet()) {
            if (!first) {
                sb.append(",");
            }
            first = false;
            sb.append("\"").append(entry.getKey().toString()).append("\":");
            Object value = entry.getValue();
            if (value == null) {
                sb.append("null");
            } else if (value instanceof String) {
                sb.append("\"").append(value.toString().replace("\"", "\\\"")).append("\"");
            } else if (value instanceof Number || value instanceof Boolean) {
                sb.append(value.toString());
            } else {
                sb.append("\"").append(value.toString().replace("\"", "\\\"")).append("\"");
            }
        }
        sb.append("}");
        return sb.toString();
    }
    
    public static void clearRetryStatistics() {
        retryCounters.clear();
        lastRetryTime.clear();
    }
}