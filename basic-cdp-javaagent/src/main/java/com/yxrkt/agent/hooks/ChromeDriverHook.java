package com.yxrkt.agent.hooks;

import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.devtools.DevTools;
import org.openqa.selenium.devtools.v85.network.Network;
import org.openqa.selenium.devtools.v85.page.Page;
import org.openqa.selenium.devtools.v85.runtime.Runtime;
import org.openqa.selenium.devtools.v85.log.Log;
import com.yxrkt.agent.bus.EventBus;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;
import java.lang.reflect.Field;
import java.util.HashSet;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

public class ChromeDriverHook {
    private static final Set<Object> initializedDrivers = ConcurrentHashMap.newKeySet();
    private static final Set<Object> knownDrivers = ConcurrentHashMap.newKeySet();

    public static void checkForNewDrivers() {
        try {
            // Use reflection to find ChromeDriver instances in the heap
            // This is a simple approach that looks for active threads that might be holding ChromeDriver references
            ThreadMXBean threadBean = ManagementFactory.getThreadMXBean();
            Thread[] threads = new Thread[threadBean.getThreadCount()];
            Thread.enumerate(threads);
            
            for (Thread thread : threads) {
                if (thread != null && thread.getName().contains("main")) {
                    // For now, we'll rely on a simpler approach
                    // The monitoring will be triggered when drivers are actually used
                }
            }
        } catch (Exception e) {
            // Ignore reflection errors
        }
    }
    
    public static void registerDriver(ChromeDriver driver) {
        if (driver != null && !initializedDrivers.contains(driver)) {
            initializeCDP(driver);
            initializedDrivers.add(driver);
        }
    }

    private static void initializeCDP(ChromeDriver chromeDriver) {
        try {
            DevTools devTools = chromeDriver.getDevTools();
            devTools.createSession();

            // Enable CDP domains
            devTools.send(Network.enable(java.util.Optional.empty(), java.util.Optional.empty(), java.util.Optional.empty()));
            devTools.send(Page.enable());
            devTools.send(Runtime.enable());
            devTools.send(Log.enable());

            // Add event listeners
             devTools.addListener(Network.requestWillBeSent(), request -> {
                 EventBus.bus().cdp("Network.requestWillBeSent", chromeDriver, j -> {
                     j.add("requestId", request.getRequestId().toString());
                     j.add("url", request.getRequest().getUrl());
                     j.add("method", request.getRequest().getMethod());
                     return null;
                 });
             });

             devTools.addListener(Network.responseReceived(), response -> {
                 EventBus.bus().cdp("Network.responseReceived", chromeDriver, j -> {
                     j.add("requestId", response.getRequestId().toString());
                     j.add("url", response.getResponse().getUrl());
                     j.add("status", response.getResponse().getStatus());
                     return null;
                 });
             });

             devTools.addListener(Page.loadEventFired(), event -> {
                 EventBus.bus().cdp("Page.loadEventFired", chromeDriver, j -> null);
             });

             devTools.addListener(Runtime.consoleAPICalled(), console -> {
                 EventBus.bus().cdp("Runtime.consoleAPICalled", chromeDriver, j -> {
                     j.add("type", console.getType().toString());
                     j.add("args", console.getArgs().toString());
                     return null;
                 });
             });

             devTools.addListener(Log.entryAdded(), log -> {
                 EventBus.bus().cdp("Log.entryAdded", chromeDriver, j -> {
                     j.add("level", log.getLevel().toString());
                     j.add("text", log.getText());
                     return null;
                 });
             });

            System.out.println("[CDP Agent] CDP initialized for ChromeDriver instance");
        } catch (Exception e) {
            System.err.println("[CDP Agent] Failed to initialize CDP: " + e.getMessage());
            e.printStackTrace();
        }
    }
}
