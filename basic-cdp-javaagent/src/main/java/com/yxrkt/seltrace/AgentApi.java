package com.yxrkt.seltrace;

import java.util.Map;

public final class AgentApi {
    public static void setTestId(String id) {
        try {
            Class<?> eb = Class.forName("com.yxrkt.agent.bus.EventBus");
            Object bus = eb.getMethod("bus").invoke(null);
            eb.getMethod("setTestId", String.class).invoke(bus, id);
        } catch (Throwable ignored) {}
    }
    
    public static void setTestContext(String key, Object value) {
        try {
            Class<?> eb = Class.forName("com.yxrkt.agent.bus.EventBus");
            Object bus = eb.getMethod("bus").invoke(null);
            eb.getMethod("setTestContext", String.class, Object.class).invoke(bus, key, value);
        } catch (Throwable ignored) {}
    }
    
    public static void setGlobalConfig(String key, Object value) {
        try {
            Class<?> eb = Class.forName("com.yxrkt.agent.bus.EventBus");
            Object bus = eb.getMethod("bus").invoke(null);
            eb.getMethod("setGlobalConfig", String.class, Object.class).invoke(bus, key, value);
        } catch (Throwable ignored) {}
    }
    
    public static void setDynamicData(String key, Object value) {
        try {
            Class<?> eb = Class.forName("com.yxrkt.agent.bus.EventBus");
            Object bus = eb.getMethod("bus").invoke(null);
            eb.getMethod("setDynamicData", String.class, Object.class).invoke(bus, key, value);
        } catch (Throwable ignored) {}
    }
    
    public static void clear() { 
        setTestId(null);
        try {
            Class<?> eb = Class.forName("com.yxrkt.agent.bus.EventBus");
            Object bus = eb.getMethod("bus").invoke(null);
            eb.getMethod("clearTestContext").invoke(bus);
        } catch (Throwable ignored) {}
    }
}
