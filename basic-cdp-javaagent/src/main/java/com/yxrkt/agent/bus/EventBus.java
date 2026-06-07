package com.yxrkt.agent.bus;

import com.yxrkt.agent.Config;

import java.io.*;
import java.time.Instant;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Function;

public class EventBus {
    private static EventBus INSTANCE;
    private PrintWriter out;
    private File outFile;
    private long maxBytes = 5 * 1024 * 1024; // 5 MB default rollover
    private int rollIndex = 0;
    private final ThreadLocal<String> testIdTL = new ThreadLocal<>();
    private final Map<String,String> sessionIds = new ConcurrentHashMap<>();
    private final ThreadLocal<Map<String,Object>> testContextTL = new ThreadLocal<>();
    private final Map<String,Object> globalConfig = new ConcurrentHashMap<>();
    private final ThreadLocal<Map<String,Object>> dynamicDataTL = new ThreadLocal<>();

    private EventBus() throws Exception {
        File f = new File(Config.outPath());
        if (f.getParentFile()!=null) f.getParentFile().mkdirs();
        this.outFile = f;
        this.out = new PrintWriter(new BufferedWriter(new FileWriter(f, true)), true);
    }

    private synchronized void checkRotate() {
        try {
            if (outFile==null) return;
            if (!outFile.exists()) return;
            if (outFile.length() >= maxBytes) {
                out.close();
                File dst = new File(outFile.getParentFile(), outFile.getName() + "." + (++rollIndex));
                outFile.renameTo(dst);
                this.outFile = new File(outFile.getPath());
                this.out = new PrintWriter(new BufferedWriter(new FileWriter(outFile, true)), true);
            }
        } catch (Throwable t) { /* ignore rotation failures */ }
    }


    public static synchronized void init() {
        if (INSTANCE==null) try { INSTANCE = new EventBus(); } catch (Exception e) { throw new RuntimeException(e); }
    }

    public static synchronized EventBus bus() { return INSTANCE; }

    public void setMaxBytes(long bytes) { this.maxBytes = bytes; }

    public void setTestId(String id) { testIdTL.set(id); }
    private String testId() { return testIdTL.get(); }
    
    // Test context and configuration tracking
    public void setTestContext(String key, Object value) {
        Map<String,Object> context = testContextTL.get();
        if (context == null) {
            context = new ConcurrentHashMap<>();
            testContextTL.set(context);
        }
        context.put(key, value);
    }
    
    public void setGlobalConfig(String key, Object value) {
        globalConfig.put(key, value);
    }
    
    public void setDynamicData(String key, Object value) {
        Map<String,Object> data = dynamicDataTL.get();
        if (data == null) {
            data = new ConcurrentHashMap<>();
            dynamicDataTL.set(data);
        }
        data.put(key, value);
    }
    
    public Map<String,Object> getTestContext() {
        return testContextTL.get();
    }
    
    public Map<String,Object> getGlobalConfig() {
        return globalConfig;
    }
    
    public Map<String,Object> getDynamicData() {
        return dynamicDataTL.get();
    }
    
    public void clearTestContext() {
        testContextTL.remove();
        dynamicDataTL.remove();
    }
    
    private String mapToJsonString(Map<String,Object> map) {
        if (map == null || map.isEmpty()) {
            return "{}";
        }
        
        StringBuilder sb = new StringBuilder();
        sb.append("{");
        boolean first = true;
        for (Map.Entry<String,Object> entry : map.entrySet()) {
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
            } else {
                sb.append("\"").append(value.toString().replace("\"", "\\\"")).append("\"");
            }
        }
        sb.append("}");
        return sb.toString();
    }

    public long startStep(String kind, String driverId, String target, String meta) {
        long start = System.nanoTime();
        Json builder = Json.obj()
            .put("@t", Instant.now().toString())
            .put("evt", "step.start")
            .put("kind", kind)
            .put("driverId", driverId)
            .put("target", target)
            .put("meta", meta)
            .put("testId", testId())
            .put("thread", Thread.currentThread().getName());
        
        // Add test context if available
        Map<String,Object> context = getTestContext();
        if (context != null && !context.isEmpty()) {
            builder.put("testContext", mapToJsonString(context));
        }
        
        // Add global configuration if available
        if (!globalConfig.isEmpty()) {
            builder.put("config", mapToJsonString(globalConfig));
        }
        
        // Add dynamic data if available
        Map<String,Object> dynamicData = getDynamicData();
        if (dynamicData != null && !dynamicData.isEmpty()) {
            builder.put("dynamicData", mapToJsonString(dynamicData));
        }
        
        emit(builder);
        return start;
    }

    public void endStep(long start, String kind, String driverId, String target, Throwable t) {
        long durMs = Math.max(0, (System.nanoTime() - start) / 1_000_000);
        Json builder = Json.obj()
            .put("@t", Instant.now().toString())
            .put("evt", t==null ? "step.ok" : "step.err")
            .put("kind", kind)
            .put("driverId", driverId)
            .put("target", target)
            .put("durationMs", durMs)
            .put("error", t==null ? null : (t.getClass().getName() + ": " + t.getMessage()))
            .put("testId", testId())
            .put("thread", Thread.currentThread().getName());
        
        // Add test context if available
        Map<String,Object> context = getTestContext();
        if (context != null && !context.isEmpty()) {
            builder.put("testContext", mapToJsonString(context));
        }
        
        // Add dynamic data if available
        Map<String,Object> dynamicData = getDynamicData();
        if (dynamicData != null && !dynamicData.isEmpty()) {
            builder.put("dynamicData", mapToJsonString(dynamicData));
        }
        
        emit(builder);
    }

    public EventBus cdp(String name, Object driver, Function<com.eclipsesource.json.JsonObject,Object> enricher) {
        System.out.println("[CDP Agent] EventBus.cdp called with name: " + name + " from thread: " + Thread.currentThread().getName());
        System.out.flush();
        String driverId = driver==null ? "na" : Integer.toHexString(System.identityHashCode(driver));
        sessionIds.computeIfAbsent(driverId, k -> "na");
        com.eclipsesource.json.JsonObject j = com.eclipsesource.json.Json.object()
            .add("@t", Instant.now().toString())
            .add("evt", "cdp")
            .add("cdp", name)
            .add("driverId", driverId)
            .add("sessionId", sessionIds.get(driverId))
            .add("testId", testId())
            .add("thread", Thread.currentThread().getName());
        System.out.println("[CDP Agent] EventBus.cdp about to call enricher");
        try {
            enricher.apply(j);
            System.out.println("[CDP Agent] EventBus.cdp enricher completed successfully");
        } catch (Throwable t) {
            System.out.println("[CDP Agent] EventBus enricher exception: " + t.getMessage());
            t.printStackTrace();
        }
        System.out.println("[CDP Agent] EventBus.cdp about to emit: " + j.toString());
        System.out.flush();
        emit(new Json(j));
        System.out.println("[CDP Agent] EventBus.cdp completed");
        System.out.flush();
        return this;
    }

    public EventBus directEmit(String name, Object driver, Function<com.eclipsesource.json.JsonObject,Object> enricher) {
        System.out.println("[CDP Agent] EventBus.directEmit called with name: " + name);
        try {
            String driverId = driver==null ? "na" : Integer.toHexString(System.identityHashCode(driver));
            sessionIds.computeIfAbsent(driverId, k -> "na");
            
            com.eclipsesource.json.JsonObject j = com.eclipsesource.json.Json.object()
                .add("@t", Instant.now().toString())
                .add("evt", "selenium")
                .add("action", name)
                .add("driverId", driverId)
                .add("sessionId", sessionIds.get(driverId))
                .add("testId", testId())
                .add("thread", Thread.currentThread().getName());
            
            if (enricher!=null) {
                enricher.apply(j);
            }
            emit(new Json(j));
            System.out.println("[CDP Agent] EventBus.directEmit completed for: " + name);
        } catch (Exception e) {
            System.out.println("[CDP Agent] EventBus.directEmit error: " + e.getMessage());
            e.printStackTrace();
        }
        return this;
    }

    public EventBus cdp(String name, Object driver, com.eclipsesource.json.JsonObject additionalData) {
        System.out.println("[CDP Agent] EventBus.cdp called with JsonObject, name: " + name + " from thread: " + Thread.currentThread().getName());
        System.out.flush();
        String driverId = driver==null ? "na" : Integer.toHexString(System.identityHashCode(driver));
        sessionIds.computeIfAbsent(driverId, k -> "na");
        com.eclipsesource.json.JsonObject j = com.eclipsesource.json.Json.object()
            .add("@t", Instant.now().toString())
            .add("evt", "cdp")
            .add("cdp", name)
            .add("driverId", driverId)
            .add("sessionId", sessionIds.get(driverId))
            .add("testId", testId())
            .add("thread", Thread.currentThread().getName());
        
        // Merge additional data
        if (additionalData != null) {
            for (com.eclipsesource.json.JsonObject.Member member : additionalData) {
                j.add(member.getName(), member.getValue());
            }
        }
        
        System.out.println("[CDP Agent] EventBus.cdp about to emit: " + j.toString());
        System.out.flush();
        emit(new Json(j));
        System.out.println("[CDP Agent] EventBus.cdp completed");
        System.out.flush();
        return this;
    }

    public void info(String msg, Object driver) { log("info", msg, driver); }
    public void warn(String msg, Object driver) { log("warn", msg, driver); }

    private void log(String level, String msg, Object driver) {
        emit(Json.obj()
            .put("@t", Instant.now().toString())
            .put("evt", "log").put("level", level).put("msg", msg)
            .put("driverId", driver==null ? null : Integer.toHexString(System.identityHashCode(driver)))
            .put("testId", testId())
        );
    }

    private void emit(Json j) {
        synchronized(this) {
            checkRotate();
        }
        out.println(j.toString());
    }
}
